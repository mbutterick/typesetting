#lang racket/base
(require racket/generic
         racket/dict
         racket/port)
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

(define (dump x)
  (define (dump-dict x)
    (for/list ([(k v) (in-dict x)])
      (cons (dump k) (dump v))))
  (let loop ([x x])
    (cond
      [(input-port? x) (port->bytes x)]
      [(output-port? x) (get-output-bytes x)]
      [(dict? x) (dump-dict x)]
      [(list? x) (map loop x)]
      [else x])))

(define-generics xenomorphic
  (encode xenomorphic val [port])
  (decode xenomorphic [port])
  (size xenomorphic))