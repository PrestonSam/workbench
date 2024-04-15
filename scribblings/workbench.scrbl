#lang scribble/manual

@require[@for-label[workbench
                    racket/base]
         @for-syntax[racket/base
                     scribble/manual
                     racket/format
                     racket/string
                     racket/port
                     racket/function
                     workbench/condition
                     workbench/define
                     workbench/sequence
                     workbench/string
                     workbench/list
                     workbench/interactive
                     workbench/regexp
                     log-once
                     adjutor/unstable]
         syntax/parse/define]

@title{Workbench}
@author{sampr}

@defmodule[workbench]

Package Description Here

@(define-for-syntax collection-types
   `((,list?   ,in-list       "(")
     (,vector? ,in-vector     "#(")
     (,hash?   ,in-hash-pairs "#hash(")))

@(define-for-syntax (expr->comment expr)
   (or (for*/first ([seq-type (in-list collection-types)]
                    [(is-type? in-seq opener) (in-match seq-type (list is-type? in-seq opener))]
                    #:when (is-type? expr))
         (define*
           [formatted-opener (format "-> '~a" opener)]
           [closer-format "\n; "])
         (string-join (for/list ([item (in-seq expr)]) (~s item))
                      #:before-first formatted-opener
                      (~a closer-format
                          #:width (+ (string-length closer-format)
                                     (string-length formatted-opener)))
                      #:after-last ")"))
       (format "-> ~s" expr)))

@(define-syntax-parser evaluated-block
   [(_ body ...+)
    #`(RACKETBLOCK
       body ...
       (UNSYNTAX (linebreak))
       (code:comment (UNSYNTAX (elem #,(expr->comment (eval #'(begin body ...)))))))])

@defform[(define*
           id-clause ...+)
         #:grammar
         [(id-clause [id expr]
                     [(id ...+) expr])]]{
 Like @racket[define], but can take any number of identifiers and expressions.
 If multiple identifiers are expected for a single expression, then the
 expression is expected to return as many values. For example
 @evaluated-block[
 (define*
   [a 'a]
   [(b c) (values 'b 'c)]
   [d 'd])
 (list a b c d)]
}

@defproc[(in-choosing-sequence [seq sequence?]
                               [choose-proc (-> any/c integer?)]
                               [choices sequence?] ...+)
         sequence?]{
 Maps each entry of @racket[seq] to the next entry of one of the list of @racket[choices].
 Each entry of @racket[seq] is passed to @racket[choose-proc], which should produce an
 ordinal index representing which of the list of @racket[choices] the entry should be mapped to.
 For example:
 @evaluated-block[
 (require racket/list
          racket/sequence)
 (define*
   [val-types '(str num sym)]
   [(string-values numerical-values symbol-values)
    (values
     '((first-name . "Mary")
       (last-name . "Anning"))
     '((birth-year . 1799)
       (birth-month . 5)
       (birth-day . 21))
     '((country . England)
       (scientific-field . palaeontology)))])
 (sequence->list
  (in-choosing-sequence '(str str sym num num num sym)
                        (λ (key)
                          (index-of val-types key))
                        string-values
                        numerical-values
                        symbol-values))
 ]
}

@defform[(if-b condition
               [then-expr ...+]
               [else-expr ...+])]{
 Like @racket[if], but supports any number of body expressions rather than just one.
 Equivalent to:
 @racketblock[
 (if condition
     (begin
       then-expr ...)
     (begin
       else-expr ...))
 ]
}

@defform[(if-let [id condition]
                 then-expr
                 else-expr)]{
 Like @racket[if], but binds the result of @racket[condition] to @racket[id], which can then be used in both @racket[then-expr] and @racket[else-expr].
 Equivalent to:
 @racketblock[
 (let ([id condition])
   (if id
       then-expr
       else-expr))
 ]
}

@defform[(for/string for-clauses maybe-before-first body-or-break ... expr)
         #:grammar [(maybe-before-first (code:line)
                                        (code:line #:before-first str-before-first))]
         #:contracts ([str-before-first string?])]{
 Iterates like @racket[for], but the last expression in the @racket[body]s must produce a single value (not necessarily a string).
 Each of the values produced by the final expr are written to the output string using  @racket[write].
 @evaluated-block[
 (for/string ([n1 (in-range (char->integer #\a) (char->integer #\f))]
              [n2 (in-range (char->integer #\A) (char->integer #\F))])
   (format "~a = ~a, "
           (integer->char n1)
           (integer->char n2)))
 ]
}

@defform[(for/append for-clauses body-or-break ... expr)]

@defform[(define-mutable-list id init-val ...)]

@defform[(list-recur list
                     (car-id cdr-id)
                     maybe-null-clause
                     body ...+)
         #:grammar [(maybe-null-clause (code:line)
                                       (code:line #:if-null null-clause))]]

@defproc[(cons/carry [v any/c]
                     [lst list?])
         list?]{
 Useful as an alternative solution to multiple return values. For example:
 @evaluated-block[
 (foldl cons/carry
        '((useful-val other-val) 3 4)
        '(2 1))
 ]
 Is equivalent to:
 @racketblock[
 (list* (car lst)
        v
        (cdr lst))
 ]
}

@defproc[(list-update-where [lst list?]
                            [pred (-> any/c boolean?)]
                            [updater (-> any/c any/c)])
         (or/c list? false?)]

@defproc[(empty-string? [str string?])
         boolean?]{
 Returns whether the provided string contains no characters
}

@defproc[(map* [fn (-> any/c any/c)]
               [lst list?])
         list?]{
 Like @racket[map], but accepts a list of lists rather than being variadic.
      @evaluated-block[
 (map* list '((1 2 3)
              (4 5 6)
              (7 8 9)))
 ]
}

@defproc[(printf-expr [v any/c]
                      [form (or/c string? false?) #f])
         any/c]{
 Prints the provided value before returning it.
 Takes an optional printf format string that can be used in place of the default, @racket["~a\n"].
}

@defproc[(in-printing-sequence [seq sequence?]
                               [form boolean? #f])
         sequence?]{
 Returns a sequence that returns the values provided in @racket[seq], but also prints the values using @racket[printf-expr].
}

@defproc[(in-prompted-lines [msg string?]
                            [in input-port? (current-input-port)]
                            [mode symbol? 'any])
         sequence?]{
 Returns a sequence that, for each entry, prompts the user with the provided message, then reads a line from the input port.
 The sequence will be stop once the input port produces @racket[eof].
}

@defproc[(call [fn procedure?])
         any]{
 Call the provided procedure.
 Meant for use as an argument to other procedures
 @racketblock[
 (map call
      (list read read-line read-syntax)) 
 ]
}


@defproc[(hash-ref* [a-hash hash?]
                    [keys list?])
         any/c]{
 Like @racket[hash-ref], but will attempt to apply @racket[hash-ref] on the value returned the previous call, using the next key in @racket[keys].
 Equivalent to:
 @racketblock[
 (foldl (swap-args hash-ref)
        a-hash
        keys)]
}

@defproc[(string-split* [str string?]
                        [seps (listof (or/c string? regexp?))])
         list?]{
 Like @racket[string-split], but will attempt to apply @racket[string-split] on the substrings produced by the previous call, using the next separator in @racket[seps].
}

@defproc[(ask [msg string?]
              [validator-transform (-> string? any) identity]
              [err-msg string? "Please enter a valid response"])
         any]{
 Prompt the user using the provided message, offering them the ability to enter a response.
 Validate (and potentially transform) the response using @racket[validator-transform].
 If @racket[validator-transform] returns false, then display @racket[err-msg] to the user and await a response once more.
 For example:
 @evaluated-block[
 (with-input-from-string "44"
   (thunk
    (ask "Please enter a number"
         string->number
         "That's not a number")))
 ]
}

@defproc[(ask/choose [msg string?]
                     [seq sequence?]
                     [validator-transform (-> string? any) identity]
                     [err-msg string? "Please enter a valid response"])
         positive?]{
 This is kinda janky. This should clearly produce the chosen element rather than its index.
 Sort out the implementations of this so they don't need this interface so that you can fix it.
}

@defproc[(confirm [msg string?])
         boolean?]{
 Print the provided message, then ask whether the user approves.
 Returns true if the response contains either @racket["Y"] or @racket["y"]
}

@defproc[(regexp-extract [pattern (or/c regexp? pregexp?)]
                         [str string?])
         any/c]{
 Like @racket[regexp-match], but always produces a single value rather than rather than a list of matches.
 The value corresponds to the first match found by the pattern. For example:
 @evaluated-block[
 (regexp-extract #px"\\s*(\\S+)\\s*" "   contents    ")
 ]
}

@defform[(define-parameters id ...+)]{
 Define a series of parameters, each with @racket[#f] as their starting value.
 Meant to be used in tandem with @racket[define-constructor].
 Example usage:
 @racketblock[
 (define-parameters param1 param2 param3)
 (define (some-proc)
   (or (param1)
       (param2)
       (param3)))
 ]
}

@defform[(define-constructor constructor-id
           load-first ...
           (export-id ...+)
           maybe-params)
         #:grammar [(load-first (code:line #:load-first (parameter-id ...+) constructor))
                    (maybe-param-clause (code:line)
                                        (code:line (param-clause ...)))
                    (params (code:line id)
                            (code:line [id value]))
                    (constructor (code:line (id (parameter-value ...) procedure-id ...+))
                                 (code:line (id procedure-id ...+)))]]{
 This one is pretty difficult to explain
 You define your params (why not use define-parameters?)
 Reference them in your functions
 Put this macro at the bottom. Define which params are used in which functions.
 This will produce a macro that, when invoked, will behave like require, importing the given functions to the new scope.
 You have to provide the value of each parameter, too - so that the functions have the appropriate value.
}

@defproc[(string-char-count [str string?]
                            [find-char char?]
                            [start positive? 0]
                            [stop (or/c positive false?) #f])
         positive?]{
 Count the number of instances of @racket[find-char] in the string, ranging from @racket[start] to @racket[stop].
}

@defproc[(string-pos->row&column [str string?]
                                 [pos positive?])
         (values positive? positive?)]
