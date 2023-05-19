#lang racket/base

(provide call)

(define (call fn . args)
  (apply fn args))
