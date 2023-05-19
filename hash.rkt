#lang racket/base

(provide swap-args
         hash-ref*
         hash-ref/transform)

(define ((swap-args fn) a b)
  (fn b a))

(define (hash-ref* a-hash keys)
  (foldl (swap-args hash-ref)
         a-hash
         keys))

(define (hash-ref/transform hash key proc)
  (define found (hash-ref hash key #f))
  (and found
       (proc found)))
