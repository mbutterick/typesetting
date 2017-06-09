#lang restructure/racket
(provide (all-defined-out))
(require "number.rkt")

(define (resolveLength length stream parent)
  (cond
    [(number? length) length]
    [(procedure? length) (length parent)]
    [(and parent (symbol? length) (hash-ref parent length))] ; treat as key into RStruct parent
    [(and stream (is-a? length Number) (send length decode stream))]
    [else (raise-argument-error 'resolveLength "fixed-size item" length)]))