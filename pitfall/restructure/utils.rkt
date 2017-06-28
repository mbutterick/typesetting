#lang restructure/racket
(provide (all-defined-out))
(require "number.rkt")

(define (resolveLength length [stream #f] [parent #f])
  (cond
    [(number? length) length]
    [(procedure? length) (length parent)]
    [(and parent (symbol? length)) (ref parent length)] ; treat as key into RStruct parent
    [(and stream (NumberT? length)) (send length decode stream)]
    [else (raise-argument-error 'resolveLength "fixed-size argument" length)]))