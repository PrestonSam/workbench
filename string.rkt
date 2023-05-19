#lang racket/base

(require racket/match
         racket/function
         racket/list
         racket/string
         syntax/parse/define
         workbench/define
         threading
         (for-syntax racket/base
                     threading))

(provide for/string
         pascalcase->snakecase
         string-camelcase
         string-screaming-snakecase
         string-skewercase
         empty-string?
         string-split*
         string->table
         string-char-count
         string-pos->row&column)

(define-syntax-parser for/string
  [(_ for-clauses (~optional (~seq #:before-first before-first)) body-or-break ... expr)
   #'(let ([out-str (open-output-string (~? before-first))])
       (for for-clauses
         body-or-break ...
         (display expr out-str))
       (get-output-string out-str))])

(define (upcase-first-letter str)
  (define new-str (string-copy str))
  (string-set! new-str 0 (char-upcase (string-ref new-str 0)))
  new-str)

(define (downcase-first-letter str)
  (define new-str (string-copy str))
  (string-set! new-str 0 (char-downcase (string-ref new-str 0)))
  new-str)

(define pascalcase-word-regexp
  #px"[A-Z]?[a-z]+|[A-Z]+(?=[A-Z]|$)")

(define (pascalcase->snakecase str)
  (~> str
      (regexp-match* pascalcase-word-regexp _)
      (map string-downcase _)
      (string-join "_")))

(define (string-camelcase str)
  (~> str
      (regexp-match* pascalcase-word-regexp _)
      (map (compose upcase-first-letter string-downcase) _)
      (apply string-append _)))

(define (string-screaming-snakecase str)
  (~> str
      (regexp-match* pascalcase-word-regexp _)
      (map string-upcase _)
      (string-join "_")))

(define (string-skewercase str)
  (~> str
      (regexp-match* pascalcase-word-regexp _)
      (map string-downcase _)
      (string-join "-")))

(define (empty-string? str)
  (= (string-length str) 0))

(define (string-split* a-str seps)
  (let loop ([str a-str]
             [seps seps])
    (match seps
      ['() str]
      [(cons sep seps)
       (map (curryr loop seps)
            (string-split str sep))])))

(define (string->table a-str)
  (string-split* a-str '("\n" "\t")))

(define (string-char-count str find-char [start 0] [stop #f])
  (for/sum ([char (in-string str start stop 1)]
            #:when (eq? char find-char))
    1))

(define (string-pos->row&column str pos)
  (for/fold ([working-pos 0]
             [column-count 0]
             [row-count 0]
             #:result (values row-count column-count))
            ([line (in-lines (open-input-string str))])
    (define*
      [len (add1 #|Include newline as character|# (string-length line))]
      [new-pos (+ len working-pos)]
      [line-exceeds-pos (> (+ len working-pos) pos)])
    #:final line-exceeds-pos
    (values
     new-pos
     (add1 (- pos working-pos))
     (add1 row-count))))
