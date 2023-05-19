#lang racket/base

(provide purge-directory
         delete-directory*)

(define (purge-directory path)
  (when (directory-exists? path)
    (for-each delete-file (directory-list path #:build? #t))))

(define (delete-directory* path)
  (when (directory-exists? path)
    (purge-directory path)
    (delete-directory path)))
