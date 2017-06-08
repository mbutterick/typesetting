#lang restructure/racket
(require "decodestream.rkt" "encodestream.rkt")
(provide RStreamcoder)

(define-subclass RBase (RStreamcoder)

  (define/overment (decode stream . args)
    (unless (is-a? stream RDecodeStream)
      (raise-argument-error 'decode "RDecodeStream" stream))
    (inner (void) decode stream . args))

  (define/overment (encode stream . args)
    (when stream
      (unless (is-a? stream REncodeStream)
        (raise-argument-error 'encode "REncodeStream" stream)))
    (inner (void) encode stream  . args)))