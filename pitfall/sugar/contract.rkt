#lang racket/base
(require racket/contract)
(provide (all-defined-out))

(define (option/c x) (or/c #f x)) 