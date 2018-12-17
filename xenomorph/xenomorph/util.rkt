#lang racket/base
(require racket/dict "number.rkt" "helper.rkt" "generic.rkt")
(provide (all-defined-out))

(define (length-resolvable? x)
  (or (not x) (symbol? x) (xenomorphic? x) (procedure? x) (exact-nonnegative-integer? x)))

(define (resolve-length x port #:parent [parent #f])
  (cond
    [(not x) #f]
    [(exact-nonnegative-integer? x) x]
    [(procedure? x) (x parent)]
    [(and parent (symbol? x)) (dict-ref parent x)]
    [(and port (x:int? x)) (decode x port)]
    [else (raise-argument-error 'resolve-length "fixed-size argument" x)]))