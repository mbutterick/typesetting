#lang restructure/racket
(require "decodestream.rkt" "encodestream.rkt")
(provide RStreamcoder)

(define-subclass RBase (RStreamcoder)

  (define/overment (decode x . args)
    (let loop ([x x])
      (cond
        [(bytes? x) (loop (open-input-bytes x))]
        [(input-port? x) (loop (make-object RDecodeStream x))]
        [(is-a? x RDecodeStream) (inner (void) decode x . args)]
        [else (raise-argument-error 'decode "item that can become RDecodeStream" x)])))

  (define/overment (encode x . args)
    (let loop ([x x])
      (cond
        [(output-port? x) (loop (make-object REncodeStream x))]
        [(is-a? x REncodeStream) (inner (void) encode x  . args)]
        [else (raise-argument-error 'encode "item that can become REncodeStream" x)]))))