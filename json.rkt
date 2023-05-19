#lang racket/base

(require json)

(provide file->jsexpr
         jsexpr->file)

(define (file->jsexpr file-path)
  (with-input-from-file file-path
    (λ () (read-json))))

(define (jsexpr->file file-path jsexpr #:exists [exists 'error])
  (with-output-to-file file-path
    (λ () (write-json jsexpr))
    #:exists exists))
