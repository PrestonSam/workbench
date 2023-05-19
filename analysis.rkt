#lang racket/base

(require racket/function
         racket/path
         racket/sequence
         threading)

(provide get-total-line-count)

(define (get-total-line-count parent-directory [used-extensions '(#".rkt")])
  (let loop ([parent parent-directory])
    (for/sum ([path (directory-list parent #:build? #t)])
      (cond
        [(directory-exists? path) (loop path)]
        [(ormap (curry path-has-extension? path) used-extensions)
         (with-input-from-file path
           (thunk
            (~>> (in-input-port-chars (current-input-port))
                 (sequence-filter (curry eq? #\newline) )
                 sequence-length)))]
        [else 0]))))

