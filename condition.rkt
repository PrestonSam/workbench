#lang racket/base

(require racket/block
         syntax/parse/define
         (for-syntax racket/base))

(provide ifb
         if-let
         and/0
         and-let
         false-if
         when/return
         unless/return)

(define-syntax-parser ifb
  [(_ cond:expr
      [then-body:expr ...+]
      [else-body:expr ...+])
   #'(if cond
         (block then-body ...)
         (block else-body ...))])

(define-syntax-parser if-let
  [(_ [var-name:id cond:expr]
      left:expr
      right:expr)
   #'(let ([var-name cond])
       (if var-name
           left
           right))])

(define-syntax-parser and/0
  [(_ condition:expr expr:expr)
   #'(if condition expr 0)])

(define-syntax-parser and-let
  [(_ (~or (~seq #:let [var-name:id stored-cond:expr])
           raw-cond:expr)
      other-forms ...+)
   #'(~? (let ([var-name stored-cond])
           (and var-name
                (and-let other-forms ...)))
         (and raw-cond
              (and-let other-forms ...)))]
  [(_ raw-cond:expr)
   #'raw-cond])

(define ((false-if pred) value)
  (and (not (pred value))
       value))

(define-syntax-parser when/return
  [(_ condition:expr expr:expr ...+)
   #'(let ([result condition])
       (and result
            (begin expr ... result)))])

(define-syntax-parser unless/return
  [(_ condition:expr expr:expr ...+)
   #'(let ([result condition])
       (or result
           (begin expr ... #f)))])
