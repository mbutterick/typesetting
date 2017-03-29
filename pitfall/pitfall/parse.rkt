#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")
(provide (matching-identifiers-out #rx"pf-" (all-defined-out)))

(module+ reader (provide read-syntax))

(define (read-syntax src port)
  (define parse-tree (parse (make-tokenizer src port)))
  (strip-bindings
   #`(module pitfall-parse-mod pitfall/parse
       #,parse-tree)))

(define-macro (my-mb PARSE-TREE)
  #'(#%module-begin PARSE-TREE))
(provide (rename-out [my-mb #%module-begin]))

(provide null)

(define-macro (pf-program ARG ...)
  #'(list ARG ...))

(define (pf-name str)
  (let* ([str (string-trim str "/" #:right? #f)]
         [str (regexp-replace* #px"#(\\d\\d)" str (λ (m sub) (string (integer->char (string->number sub 16)))))])
         (string->symbol str)))

(pf-name "B#45#20L")