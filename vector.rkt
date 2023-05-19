#lang racket/base

(provide vector-ref!)

(define (vector-ref! vec pos val-proc)
  (or (vector-ref vec pos)
      (let ([val (val-proc)])
        (vector-set! vec pos val)
        val)))
