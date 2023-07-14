#lang racket/base

(require racket/block
         racket/match
         racket/function
         racket/list
         racket/promise
         log-once
         workbench/define
         workbench/condition
         syntax/parse/define
         (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     threading))

(provide for/append
         for/list/carry
         define-mutable-list
         list-recur
         cons/carry
         list-or-false
         list-update-where
         member?
         member*?
         find/equal
         find/key
         assoc/cdr
         average
         false-if-null
         map*
         sort*
         take-up-to
         zip/2d)

(define-syntax-parser for/append
  [(_ for-clauses body-or-break ... body)
   #'(for/foldr ([lst null] #:delay)
                for-clauses
       body-or-break ...
       (append body
               (force lst)))])

(define-syntax-parser for/list/carry
  [(_ ([var expr] ...)
      for-clauses
      body-or-break ...
      body)
   #:with (next-var ...) (for/list ([cur-var (in-syntax #'(var ...))])
                           (format-id #'cur-var "next-~a" #'cur-var))
   #'(block
      (define var expr) ...
      (for/list for-clauses
        body-or-break ...
        (let-values ([(val next-var ...) body])
          (set! var next-var) ...
          val)))])

(define-syntax-parser define-mutable-list
  [(_ id:id init-val ...)
   #:with add-id (format-id #'id "~a-add!" #'id)
   #:with remove-id (format-id #'id "~a-remove!" #'id)
   #:with update-id (format-id #'id "~a-update!" #'id)
   #'(begin
       (define id (list init-val ...))
       (define (add-id v) (set! id (cons v id)))
       (define (remove-id v [proc equal?]) (set! id (remove v id proc)))
       (define (update-id pred updater) (set! id (list-update-where id pred updater))))])

(define-syntax-parser list-recur
  [(_ lst:expr (car-id:id cdr-id:id)
               (~optional (~seq #:if-null null-clause))
               body:expr ...+)
   #'(let ([the-lst lst])
       (if (null? the-lst)
           (~? null-clause null)
           (let ([car-id (car the-lst)]
                 [cdr-id (cdr the-lst)])
             body ...)))])

(define (cons/carry v lst)
  (list* (car lst)
         v
         (cdr lst)))

(define (list-or-false lst)
  (match-let ([(list success lst ...) lst])
    (and success
         lst)))

(define (list-update-where lst pred updater)
  (list-or-false
   (let update-loop ([lst lst])
     (list-recur lst (fst rst) #:if-null '(#f)
                 (if (pred fst)
                     (list* #t (updater fst) rst)
                     (cons/carry fst (update-loop rst)))))))

(define (member? v lst [is-equal? equal?])
  (and (member v lst is-equal?) #t))

(define (member*? v-lst lst [is-equal? equal?])
  (for*/or ([item (in-list lst)]
            [v (in-list v-lst)])
    (is-equal? item v)))
    

(define (find/equal v lst equal?-fn #:key [key-fn identity])
  (for/first ([val (in-list lst)]
              #:when (equal?-fn (key-fn val) v))
    val))

(define (find/key v lst key-fn #:equal? [equal?-fn equal?])
  (for/first ([val (in-list lst)]
              #:when (equal?-fn (key-fn val) v))
    val))

(define (assoc/cdr v lst [equal? equal?] [default-val #f])
  (define out (assoc v lst equal?))
  (if out
      (cdr out)
      default-val))

(define (average lst #:key [key-fn identity])
  (/ (apply + (map key-fn lst)) (length lst)))

(define false-if-null
  (false-if null?))
  
(define (map* fn lst)
  (apply (curry map fn) lst))

(define-syntax-parser sort*
  [(_ lst:expr
      (~seq less-than?:expr
            (~optional (~seq #:key extract-key:expr))
            (~optional (~seq #:cache-keys? cache-keys?:expr)))
      (~seq less-than?...:expr
            (~optional (~seq #:key extract-key...:expr))
            (~optional (~seq #:cache-keys? cache-keys?...:expr))) ...)
   #'(sort (sort* lst
                  (~@ less-than?...
                      (~? (~@ #:key extract-key...))
                      (~? (~@ #:cache-keys? cache-keys?...))) ...)
           less-than?
           (~? (~@ #:key extract-key))
           (~? (~@ #:cache-keys? cache-keys?)))]
  [(_ lst:expr)
   #'lst])

(define (take-up-to lst idx)
  (cond
    [(< idx (length lst))
     (take lst idx)]
    [else
     lst]))

(define (maybe-cons v lst)
  (if v
      (cons v lst)
      lst))

(define (zip/2d lst)
  (define lst-length
    (apply max (map length lst)))
  (for/fold ([outlst (make-list lst-length null)]
             #:result (map reverse outlst))
            ([sublst (in-list lst)])
    (define*
      [padding (make-list (- lst-length (length sublst)) #f)]
      [padded-sublst (append sublst padding)])
    (map maybe-cons padded-sublst outlst)))


