#lang racket/base
(require racket/list
         racket/set
         "struct.rkt")
(provide (all-defined-out))

(define default-font-family "text")
(define default-font-size 12)
(define default-no-features (seteq))

(define-syntax-rule (define-attr-list LIST-NAME
                      [ATTR-NAME ATTR-EXPR] ...)
  (begin
    (define ATTR-NAME ATTR-EXPR) ...
    (define LIST-NAME
      (let ([names (list ATTR-NAME ...)])
        (cond
          [(check-duplicates (map attr-key-name names))
           =>
           (λ (sym) (raise-user-error 'define-attr-list "duplicate attribute name: ~a" sym))]
          [else names])))))

(define-attr-list all-attrs
  [:unknown-key (make-attr-unknown-key (gensym))]
  [:font-family (make-attr-uncased-string-key 'font-family #true default-font-family)]
  [:font-path (make-attr-path-key 'font-path)]
  [:font-bold (make-attr-boolean-key 'font-bold #true #false)]
  [:font-italic (make-attr-boolean-key 'font-italic #true #false)]
  [:font-size (make-attr-dimension-string-key 'font-size #true default-font-size)]
  [:font-features (make-attr-set-key 'font-features default-no-features)]
  [:font-features-add (make-attr-set-key 'font-features-add default-no-features)]
  [:font-features-subtract (make-attr-set-key 'font-features-subtract default-no-features)])