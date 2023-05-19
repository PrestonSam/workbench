#lang racket/base

(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/lib/function-header))

(provide define*
         define/provide)

(define-syntax-parser define*
  [(_ (~or [id:id expr:expr]
           [(idv:id ...+) expr:expr]
           [#:match match-clause:expr expr:expr]
           [#:λ header:function-header body:expr ...+]) ...+)
   #'(begin
       (~? (define id expr)
           (~? (define-values (idv ...) expr)
               (~? (match-define match-clause expr)
                   (define header body ...)))) ...)])

(define-syntax-parser define/provide
  [(_ header:function-header body ...+)
   #'(begin
      (provide header.name)
      (define header body ...))])