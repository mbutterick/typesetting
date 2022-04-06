#lang racket/base
(require "struct.rkt")
(provide (all-defined-out))

(define-syntax-rule (define-attr-list LIST-NAME
                      [ATTR-NAME ATTR-EXPR] ...)
  (begin
    (define ATTR-NAME ATTR-EXPR) ...
    (define LIST-NAME (list ATTR-NAME ...))))

(define-attr-list all-attrs
  [:font-family (attr-uncased-string-key 'font-family)]
  [:font-path (attr-path-key 'font-path)]
  [:font-bold (attr-boolean-key 'font-bold)]
  [:font-italic (attr-boolean-key 'font-italic)]
  [:font-size (attr-dimension-string-key 'font-size)])