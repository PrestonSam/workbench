#lang racket/base

(require racket/match
         syntax/parse
         (for-syntax racket/base
                     syntax/parse)
         workbench/define
         threading)


(provide hash-ref*
         hash-ref/transform
         list-with
         hash-with
         group-by/hash)


(define (hash-ref* a-hash keys [failure-result (void)])
  (foldl (if (void? failure-result) ;; Idk what the default failure result actually is, so this is a workaround
             (λ (key hash) (hash-ref hash key))
             (λ (key hash) (and hash (hash-ref hash key failure-result))))
         a-hash
         keys))


(define (hash-ref/transform hash key proc)
  (define found (hash-ref hash key #f))
  (and found
       (proc found)))


(define-match-expander list-with
  (λ (stx)
    (syntax-parse stx
      [(_ v ...+)
       #'(list _ (... ...) v ... _ (... ...))])))


(define-match-expander hash-with
  (λ (stx)
    (syntax-parse stx
      [(_ v ...+)
       #'(hash-table v ... _ (... ...))])))


(define (group-by/hash key-proc val-proc seq)
  (define*
    [out-hash (make-hash)])
  (for* ([entry seq]
         [key (in-value (key-proc entry))]
         [val (in-value (val-proc entry))])
    (~>> (hash-ref out-hash key '())
         (cons val)
         (hash-set! out-hash key)))
  out-hash)

