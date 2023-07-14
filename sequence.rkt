#lang racket/base

(require racket/function
         racket/sequence
         workbench/define)

(provide in-choosing-sequence
         in-value/cycle)

(define (in-choosing-sequence seq choose-proc . choices)
    (define choice-seqs
      (for/vector #:length (length choices)
                  ([choice (in-list choices)]
                   [pos (in-naturals)])
      (call-with-values (thunk (sequence-generate choice)) cons)))
    (define (seq-val->chosen-val seq-val)
      (define*
        [chosen-idx (choose-proc seq-val)]
        [choice (vector-ref choice-seqs chosen-idx)]
        [next? (car choice)]
        [next (cdr choice)])
      (if (next?)
          (next)
          (error 'in-choosing-sequence
                 "Value '~a' cannot be transformed as the chosen sequence with index '~a' is exhausted!"
                 seq-val
                 chosen-idx)))
    
    (sequence-map seq-val->chosen-val seq))

(define (in-value/cycle #:prefix-seq [seq (in-list '())] value)
  (in-sequences seq
                (in-cycle (in-value value))))
