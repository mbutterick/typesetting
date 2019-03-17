#lang racket/base
(require racket/match racket/dict "int.rkt" "base.rkt")
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

(define-values (PropertyDescriptor-prop PropertyDescriptor? _)
  (make-impersonator-property 'PropertyDescriptor))

(define (PropertyDescriptor [opts (make-hash)])
  (define mh (make-hash))
  (for ([(k v) (in-hash opts)])
    (hash-set! mh k v))
  (impersonate-hash mh
                    (λ (h k) (values k (λ (h k v) v)))
                    (λ (h k v) (values k v))
                    (λ (h k) k)
                    (λ (h k) k)
                    PropertyDescriptor-prop
                    #true))

(module+ test
  (require rackunit)
  (define pd (PropertyDescriptor))
  (hash-set! pd 'k 42)
  (check-equal? (hash-ref pd 'k) 42)
  (check-true (PropertyDescriptor? pd))
  (check-true (hash? pd)))