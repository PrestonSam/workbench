#lang racket/base

(require racket/bool
         racket/match
         racket/function
         racket/sequence
         racket/string
         threading
         workbench/define
         workbench/condition)

(provide return
         reprintf
         printfln
         in-printing-sequence
         in-prompted-lines
         predicate->validator-transform
         ask
         ask/choose
         ask/list
         confirm
         collect-input)

(define (return)
  (write-char #\return))

(define (reprintf form . v)
  (return)
  (apply printf form v))

(define (printfln form . v)
  (apply printf form v)
  (newline))

(define ((printf-expr [form "~a\n"]) v)
  (printf form v)
  v)

(define (in-printing-sequence seq [form #f])
  (sequence-map (printf-expr form) seq))

(define (in-prompted-lines msg [in (current-input-port)] [mode 'any])
  (in-producer (thunk
                (displayln msg)
                (read-line in mode))
               eof-object?))

(define ((predicate->validator-transform proc) v)
  (and (proc v)
       v))

(define (ask msg [valid?/transform identity] [escape-string #f] [err-msg "Please enter a valid response"])
  (displayln msg)

  (define*
    [#:λ (is-escape-string? value)
      (and escape-string
           (equal? escape-string value))]
    [true? (negate false?)])

  (let loop ([first-attempt #t])
    (unless first-attempt
      (displayln err-msg))

    (match (read-line)
      [(? is-escape-string?) #f]
      [(app valid?/transform (? true? transformed)) transformed]
      [else (loop #f)])))

(define ((sequence-index? seq) str)
  (let ([n (string->number str)])
    (and (number? n)
         (positive? n)
         (<= n (sequence-length seq))
         n)))

(define (ask/choose msg seq
                    [escape-string #f]
                    #:key [extract-key identity]
                    [err-msg "Please enter a valid response"])
  (define*
    [#:λ (msg-suffix prefix)
     (if msg
         (format "~a\n~a" msg prefix)
         prefix)]
    [prompt (~> (for/list ([entry seq]
                           [n (in-naturals 1)])
                  (format "  ~a | ~a" n (extract-key entry)))
                (string-join "\n")
                msg-suffix)]
    [response
     (ask prompt (sequence-index? seq) escape-string)])
  (and response
       (sequence-ref seq (sub1 response))))

;; Prompt the user for a series of values, read using `read-proc`
;; Each of the values in `lst` is converted into a prompt using `prompt-proc` and shown to the user
;; The user's responses are collected as they give them
;; If a user is dissatisfied with an input they have given, they can type "undo" to return to the previous input
;; This operation can be repeated any number of times.
;; The user can also end the operation prematurely by typing "end". This will return whatever values they have submitted thus far.
(define (ask/list lst prompt-proc [read-proc read-line])
  (let loop ([input-items lst]
             [visited-items null]
             [output null])
    (match input-items
      [`(,input-item . ,other-inputs)
       (displayln (prompt-proc input-item))
       (match* ((read-proc) visited-items)
         [("end" _) (reverse output)]
         [("undo" `(,visited-item . ,other-visited))
          (loop `(,visited-item . ,input-items)
                other-visited
                (cdr output))]
         [(value _)
          (loop other-inputs
                `(,input-item . ,visited-items)
                `(,value . ,output))])]
      ['() (reverse output)])))

(define (confirm msg)
  (printf "~a (y/n)\n" msg)
  (regexp-match? #px"[yY]" (read-line)))

(define (collect-input [read-fn read-line])
  (let loop ([prev (read-fn)])
    (match prev
      ["end" null]
      [val
       (match (read-fn)
         ["undo" (loop (read-fn))]
         [next (cons val (loop next))])])))
