#lang racket/base
(require racket/set)
(provide (all-defined-out))

(define default-font-family "text")
(define default-font-size 12)
(define default-no-features (seteq))
(define default-page-size "letter")
(define default-page-orientation "tall")

(struct no-value ())
(define no-value-signal (no-value))

