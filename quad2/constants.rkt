#lang racket/base
(require "struct.rkt")
(provide (all-defined-out))

(define default-font-family "text")
(define default-font-size 12)

(define-syntax-rule (define-attr-list LIST-NAME
                      [ATTR-NAME ATTR-EXPR] ...)
  (begin
    (define ATTR-NAME ATTR-EXPR) ...
    (define LIST-NAME (list ATTR-NAME ...))))

(define-attr-list all-attrs
  [:unknown-key (make-attr-unknown-key (gensym))]
  [:font-family (make-attr-uncased-string-key 'font-family #true default-font-family)]
  [:font-path (make-attr-path-key 'font-path)]
  [:font-bold (make-attr-boolean-key 'font-bold #true #false)]
  [:font-italic (make-attr-boolean-key 'font-italic #true #false)]
  [:font-size (make-attr-dimension-string-key 'font-size #true default-font-size)])