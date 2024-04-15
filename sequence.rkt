#lang racket/base

(require racket/function
         racket/sequence
         workbench/define)

(provide in-choosing-sequence
         in-value/cycle
         sequence-take)

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

#;(define (in-folder path)
  ())

(define (sequence-take seq limit)
  (let ([count 0])
    (stop-before
     seq
     (λ (_)
       (or (= limit count)
           (begin0 #f (set! count (add1 count))))))))

(define (sequence-drop seq limit)
    (define* [(more? next) (sequence-generate seq)])
    (for ([n (in-range limit)]) (next))
    (in-producer (λ () (if (more?) (next) eof))
                 eof))
