#lang racket/base

(require csv-reading
         csv-writing)

(provide csv-file->list
         list->csv-file)

(define (csv-file->list path)
  (with-input-from-file path
    (λ () (csv->list (current-input-port)))))

(define (list->csv-file path lst)
  (with-output-to-file path
    (λ () (display-table lst))
    #:exists 'truncate))
