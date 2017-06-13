#lang restructure/racket
(provide (all-defined-out))
(require "number.rkt")

(define (resolveLength _length stream parent)
  (cond
    [(number? _length) _length]
    [(procedure? _length) (_length parent)]
    [(and parent (symbol? _length) (hash-ref parent _length))] ; treat as key into RStruct parent
    [(and stream (is-a? _length Number) (send _length decode stream))]
    [else (raise-argument-error 'resolveLength "fixed-size item" _length)]))