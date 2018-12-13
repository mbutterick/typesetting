#lang racket/base
(require racket/generic
         racket/dict
         racket/port)
(provide (all-defined-out))

(define (->input-port arg)
  (cond
    [(bytes? arg) (open-input-bytes arg)]
    [(input-port? arg) arg]
    [else (raise-argument-error '->input-port "byte string or input port" arg)]))

(define (dump x)
  (cond
    [(input-port? x) (port->bytes x)]
    [(output-port? x) (get-output-bytes x)]
    [(dict? x) (for/list ([(k v) (in-dict x)])
                 (cons (dump k) (dump v)))]
    [(list? x) (map dump x)]
    [else x]))

(define (pos p [new-pos #f])
  (when new-pos
    (file-position p new-pos))
  (file-position p))

(struct xbase (pre-encode post-decode) #:transparent #:mutable)

(define-generics xenomorphic
  (encode xenomorphic val [port] #:parent [parent])
  (decode xenomorphic [port] #:parent [parent])
  (size xenomorphic [item] #:parent [parent]))

(define (finalize-size size)
  (cond
    [(void? size) 0]
    [(and (integer? size) (not (negative? size))) size]
    [else (raise-argument-error 'size "nonnegative integer" size)]))

(struct lazy-thunk (proc) #:transparent)