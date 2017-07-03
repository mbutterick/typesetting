#lang reader (submod "racket.rkt" reader)
(provide (all-defined-out) (rename-out [resolveLength resolve-length]))
(require "number.rkt")

(define (resolveLength len-arg [stream #f] [parent #f])
  (cond
    [(not len-arg) #f]
    [(number? len-arg) len-arg]
    [(procedure? len-arg) (len-arg parent)]
    [(and parent (key? len-arg)) (ref parent len-arg)] ; treat as key into RStruct parent
    [(and stream (NumberT? len-arg)) (send len-arg decode stream)]
    [else (raise-argument-error 'resolveLength "fixed-size argument" len-arg)]))