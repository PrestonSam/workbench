#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base))

(provide define-user-exns
         try
         throw)

(define-syntax-parser define-user-exns
  [(_ name ...+)
   #'(begin (struct name exn:fail:user ()) ...)])

(define-syntax-parser try ; This is a macro to prevent unnecessary evaluation of error parameters
  [(_ expr:expr exn:expr (~optional (~seq #:continuation continuation)) msg:expr msg-args:expr ...)
   #'(or expr
         (throw exn #:continuation (~? continuation (current-continuation-marks)) msg msg-args ...))])

(define (throw exn #:continuation [continuation (current-continuation-marks)] msg . msg-args)
  (raise
   (exn (apply format msg msg-args)
        continuation)))
