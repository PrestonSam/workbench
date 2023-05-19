#lang racket/base

(require racket/match
         racket/function
         syntax/parse/define
         rackunit
         (for-syntax racket/base
                     racket/syntax
                     racket/sequence
                     racket/function
                     threading))

(provide struct/accessors
         define/test)

#;(define-for-syntax ~?*
    (syntax-parser
      [(_ body)
       #'((... ~?) body)]
      [(_ body1 body2)
       #'((... ~?) body1 body2)]
      [(_ body others ...+)
       #'((... ~?) body (~?* others ...))]))

  ; This is beyond me at the moment
#;(define-syntax-parser syntax/cases
    #:literals (~?*)
    [(_ (~?* ft-body ...+) others ...)
     #`(#,(~?* #'(ft-body ...)) (~@ (syntax/cases others ...)))]
    [(_ body others ...+)
     #`(body (~@ (syntax/cases others ...)))]
    [(_ body)
     #`body])
  
(define-syntax-parser struct/accessors
  [(_ id:id (~optional maybe-super:id) (field* ...) struct-option:keyword ...)
   #:with accessor-ids (for/list ([field (in-syntax #'(field* ...))])
                         #``(field . ,#,(format-id #'id "~a-~a" #'id field)))
   #`(begin
       (struct id (~? maybe-super) (field* ...)
         struct-option ...)
       (define #,(format-id #'id "~a-accessors" #'id)
         (list (~@ . accessor-ids))))])

#;(define-provide-syntax (struct-out/accessors struct-name . struct-names)
    ; make it so that it's #'(combine-out (struct-out a) a-accessors) ...
    #f)

#;(define-syntax-parser generate-threader
    [(_ original-expr:expr root:expr)
     #'(let recur ([expr1 #'original-expr])
         (log-once #'original-expr #'root #:newline #t)
         (syntax-parse (log-expr expr1)
           [(pre-expr:expr (... ...) (~datum root) post-expr:expr (... ...))
            (log-defs #'(pre-expr (... ...)) #'(post-expr (... ...)))
            #'(#:root
               (~> root
                   (pre-expr (... ...) _ post-expr (... ...))))]
           [(expr1:expr expr2:expr (... ...+))
            #:with children (begin (log-defs #'expr1 #'(expr2 (... ...)))
                                   (datum->syntax #'expr1 (map recur (syntax->list #'(expr1 expr2 (... ...))))))
            
            (log-defs #'children)
            (syntax-parse #'children
              [(pre-expr:expr (... ...) (#:root root-expr:expr) post-expr:expr (... ...))
               #'(#:root ((~@ . root-expr) (pre-expr (... ...) _ post-expr (... ...))))]
              [expr:expr
               #'expr])]
           [expr:expr
            #'expr]))])

#;(define-syntax-parser expr->threaded
    [(_ original-expr:expr root:expr)
     (syntax-parse (generate-threader #'original-expr #'root)
       [(#:root expr:expr)
        #'expr])])
              
#;(expr->threaded
   (a b (e f (g h) i j) c)
   (g h))

(define-for-syntax (expand-test-body name stx)
  (define expanded-test
    (syntax-parse stx
      [(msg:str (args ...) should-be:expr)
       #`(check-equal? (#,name args ...)
                       should-be
                       msg)]
      [(msg:str (args ...) #:throws throw-pred:expr)
       #`(check-exn throw-pred
                    (thunk (#,name args ...))
                    msg)]
      [(msg:str (args ...) #:values [should-be:expr ...+])
       #`(check-equal? (call-with-values (thunk (#,name args ...)) list)
                       (list should-be ...)
                       msg)]
      [(msg:str (args ...) #:pred pred:expr)
       #`(check-pred pred
                     (#,name args ...)
                     msg)]))
  #`(test-case
     #,(format "Tests for '~a'" (syntax-e name))
     #,expanded-test))

(define-syntax-parser define/test
  [(_ (name:id args ...)
      [body ...+]
      [(msg:str (test-args ...)
                (~or (~seq #:throws throw-pred:expr)
                     (~seq #:values [should-be-v:expr ...+])
                     (~seq #:pred pred:expr)
                     should-be:expr)) ...+])
   #`(begin
       (define (name args ...)
         body ...)
       (module+ test
         (test-case #,(format "Tests for '~a'" (syntax-e #'name))
          (~? (check-equal? (name test-args ...) should-be msg)
              (~? (check-exn throw-pred (thunk name test-args ...) msg)
                  (~? (check-equal? (call-with-values (thunk (name test-args ...)) list)
                                    (list should-be-v ...)
                                    msg)
                      (check-pred pred (name test-args ...) msg)))))
         ...))])
