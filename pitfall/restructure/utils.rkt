#lang restructure/racket
(provide (all-defined-out))
(require "number.rkt")

(define (resolveLength len stream parent)
  (cond
    [(number? len) len]
    [(procedure? len) (len parent)]
    [(and parent (symbol? len) (hash-ref parent len))] ; treat as key into RStruct parent
    [(and stream (Number? len) (send len decode stream))]
    [else (raise-argument-error 'resolveLength "not a fixed size" len)]))