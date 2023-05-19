#lang racket/base

(require racket/sequence
         syntax/parse
         syntax/parse/define
         workbench/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/define
                     racket/sequence
                     syntax/datum
                     workbench/sequence
                     threading))

(provide define-parameters
         define-constructor)

(define-syntax-parser define-parameters
  [(_ id:id ...+)
   #'(define* [id (make-parameter #f)] ...)])

(begin-for-syntax
  (define-syntax-class param-clause
    #:attributes (id val preloaded)
    (pattern id:id
             #:with val #'#f
             #:attr preloaded #f)
    (pattern [id:id val:expr]
             #:attr preloaded #t))
  (define-syntax-class ctor
    #:attributes (id (param-value 1) (fn-id 1))
    (pattern (id:id (param-value:expr ...) fn-id ...+))
    (pattern (id:id fn-id ...+)
             #:with (param-value:expr ...) #'())))

(define-syntax-parser make-constructor-macro
  [(_ constructor-id:id
      (((load-param-clause:id ...+) load-ctor:ctor) ...)
      (export-id:id ...+)
      (param-clause:param-clause ...))
   #`(begin
       (load-ctor.id (load-ctor.param-value ...) load-ctor.fn-id ...) ...
       (define-syntax-parser constructor-id
         [ctor:ctor
          
          #:with (paired-export-id (... ...)) (for/list ([ctor-fn-id (sequence-map syntax-e (in-syntax #'(ctor.fn-id (... ...))))])
                                                (for/first ([export-fn-id (in-syntax #'(export-id ...))]
                                                            #:when (eq? ctor-fn-id (syntax-e export-fn-id)))
                                                  export-fn-id))
          
          #:with ctor-let-names (for/list ([id (in-syntax #'(param-clause.id ...))])
                                  (format-id #'id "~a-~a" #'ctor.id id))
          
          #:with ctor-define-statements (for/list ([def-id (in-syntax #'ctor-let-names)]
                                                   [val (in-choosing-sequence (list #,@(datum (param-clause.preloaded ...)))
                                                                              (λ (preloaded) (if preloaded 0 1))
                                                                              (in-syntax #'(param-clause.val ...))
                                                                              (in-syntax #'(ctor.param-value (... ...))))])
                                          #`[#,def-id #,val])
          
          #:with ctor-param-pairs (for/list ([ctor-let-name (in-syntax #'ctor-let-names)]
                                             [param-id (in-syntax #'(param-clause.id ...))])
                                    #`[#,param-id #,ctor-let-name])
          
          #:with load-param-pairs (for/list ([id (in-syntax #'(load-param-clause ... ...))]
                                             [val (in-syntax #'(load-ctor.fn-id ... ...))])
                                    #`[#,id #,val])
          
          #'(define*
              ((... ~@) . ctor-define-statements)
              [ctor.fn-id (make-keyword-procedure
                           (λ (kws kw-args . rest)
                             (parameterize (((... ~@) . ctor-param-pairs)
                                            ((... ~@) . load-param-pairs))
                               (keyword-apply paired-export-id kws kw-args rest))))]
              (... ...))]))])

(define-syntax-parser define-constructor
  [(_ constructor-id:id
      (~seq #:load-first ((load-param-clause:id ...+)
                          load-ctor:ctor)) ...
      (export-id:id ...+)
      (~optional (param-clause:param-clause ...)))
   #'(make-constructor-macro constructor-id
      (((load-param-clause ...) load-ctor) ...)
      (export-id ...)
      (~? (param-clause ...) ()))])

#;(module+ test
  (module+ procmod
    (define-values (false-proc true-proc)
      (values
       (λ () #f)
       (λ () #t)))

    (define-constructor proc-ctor
      (false-proc true-proc))
    (provide proc-ctor))
  (require 'procmod)
  (proc-ctor true-proc false-proc))
