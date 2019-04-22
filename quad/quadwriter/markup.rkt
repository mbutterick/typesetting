#lang debug racket/base
(require "lang-helper.rkt"
         (only-in "markdown.rkt" doc-proc))
(provide #%top #%datum #%app #%top-interaction
         (all-from-out "tags.rkt"))

(make-module-begin doc-proc)

(module reader racket/base
  (require "lang-helper.rkt" pollen/decode)
  (provide read-syntax)
  (define read-syntax
    (make-read-syntax 'quadwriter/markup
                      (λ (path-string ip)
                        (detect-paragraphs (syntax->datum (quad-at-reader path-string ip)))))))