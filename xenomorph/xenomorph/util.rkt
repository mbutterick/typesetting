#lang racket/base
(require racket/match racket/dict "number.rkt" "base.rkt")
(provide (all-defined-out))

(define (length-resolvable? x)
  (or (not x) (symbol? x) (xenomorphic? x) (procedure? x) (exact-nonnegative-integer? x)))

(define (resolve-length x port [parent #f])
  (match x
    [#false #false]
    [(? exact-nonnegative-integer?) x]
    [(? procedure? proc) (proc parent)]
    [(? symbol? key) #:when parent (dict-ref parent key)]
    [(? x:int?) #:when port (decode x port)]
    [_ (raise-argument-error 'resolve-length "fixed-size argument" x)]))