#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base))

(provide idempotent/expr
         idempotent/proc)

(define-syntax-parser idempotent/expr
  [(_ condition:expr)
   #:with enacted (syntax-local-lift-expression #'#f)
   #'(or enacted
         (and condition
              (set! enacted #t)
              #t))])

(define-syntax-parser idempotent/proc
  [(_ condition:expr)
   #:with enacted (syntax-local-lift-expression #'#f)
   #'(λ args
       (or enacted
           (and (apply condition args)
                (set! enacted #t)
                #t)))])

