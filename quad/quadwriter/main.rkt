#lang debug racket/base
(require pollen/tag "lang-helper.rkt")
(provide #%top #%datum #%app #%top-interaction q)

(define q (default-tag-function 'q))
(define (doc-proc strs) (apply q strs))
(make-mb doc-proc)

(module reader racket/base
  (require "lang-helper.rkt")
  (provide read-syntax)
  (define read-syntax (make-read-syntax 'quadwriter quad-at-reader)))