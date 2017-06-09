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

#|
(define-subclass RBase (RStreamcoder)

  (define/overment (decode stream-or-port . args)
    (unless (or (is-a? stream RDecodeStream) (input-port? stream-or-port))
      (raise-argument-error 'decode "RDecodeStream or input port" stream))
    (define stream (and stream-or-port
                        (if (input-port? stream-or-port)
                            (make-object RDecodeStream stream-or-port)
                            stream-or-port)))
    (inner (void) decode stream . args))

  (define/overment (encode stream-or-port . args)
    (report stream-or-port)
    (when stream-or-port
      (unless (or (is-a? stream-or-port REncodeStream) (output-port? stream-or-port))
        (raise-argument-error 'encode "REncodeStream or output port" stream-or-port)))

    (define stream (and stream-or-port
                        (if (output-port? stream-or-port)
                            (make-object REncodeStream stream-or-port)
                            stream-or-port)))
    
    (inner (void) encode stream  . args)))
|#