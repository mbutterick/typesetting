#lang racket/base
(require "struct.rkt")
(provide (all-defined-out))

(define-syntax-rule (define-attr-list LIST-NAME
                      [ATTR-NAME ATTR-EXPR] ...)
  (begin
    (define ATTR-NAME ATTR-EXPR) ...
    (define LIST-NAME (list ATTR-NAME ...))))

(define-attr-list all-attrs
  [:font-family (attr-uncased-string 'font-family)]
  [:font-path (attr-path 'font-path)]
  [:font-bold (attr-boolean 'font-bold)]
  [:font-italic (attr-boolean 'font-italic)]
  [:font-size (attr-dimension-string 'font-size)])