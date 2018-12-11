#lang racket/base
(require racket/generic)
(provide (all-defined-out))

(define (->input-port arg)
  (cond
    [(bytes? arg) (open-input-bytes arg)]
    [(input-port? arg) arg]
    [else (raise-argument-error '->port "byte string or input port" arg)]))

(define (reverse-bytes bstr) (apply bytes (reverse (bytes->list bstr))))

(define (unsigned->signed uint bits)
  (define most-significant-bit-mask (arithmetic-shift 1 (sub1 bits)))
  (- (bitwise-xor uint most-significant-bit-mask) most-significant-bit-mask))

(define (signed->unsigned sint bits)
  (bitwise-and sint (arithmetic-shift 1 bits)))

(define-generics xenomorphic
  (encode xenomorphic val [port])
  (decode xenomorphic [port])
  (size xenomorphic))