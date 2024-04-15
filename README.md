Workbench | A toolkit of high-level convenience functions
=========
This package contains many functions that perform high-level abstractions that occur commonly in my code.

The functions are categorised into files reflecting their nature - for example `regexp`, `interactive` and `list`.

The most commonly used library is `workbench/define`, which introduces a macro for chaining definitions together:
```racket
(require workbench/define)

(define*
 [a 4] ;; Define a single value
 [b a] ;; Expands to `define` macro, so definitions can reference one another
 [(c d) (values 'c 'd)] ;; Can also also expand to `define-values` macro
 [e (+ b 4)] ;; Supports expressions as usual
 [#:λ (some-fn in) ;; Syntax for (define (fn args ...) body ...) macro
    (+ in e)]
 [#:λ ((other-fn in-a) in-b) ;; Full function support including partial evaluation and varargs
    (+ in-a in-b e)])
```