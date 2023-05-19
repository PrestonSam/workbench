#lang racket/base

(require racket/match)

(provide regexp-extract)

(define (regexp-extract pattern str)
  (match (regexp-match pattern str)
    [(list _ v _ ...) v]
    [(list v) v]
    [#f #f]))
