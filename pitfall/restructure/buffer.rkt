#lang restructure/racket
(require "number.rkt" "utils.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

(define-subclass RestructureBase (Buffer [_length #xffff])
  (define/override (decode stream [parent #f])
    (define len (resolveLength _length stream parent))
    (send stream readBuffer len))

  (define/override (size [val #f] [parent #f])
    (when val (unless (bytes? val)
                (raise-argument-error 'Buffer:size "bytes" val)))
    (if val
        (bytes-length val)
        (resolveLength _length val parent)))

  (define/override (encode stream buf parent)
    (when (Number? _length)
      (send _length encode stream (bytes-length buf)))
    (send stream writeBuffer buf)))


#;(test-module
 (require "stream.rkt")
 (define stream (+DecodeStream #"\2BCDEF"))
 (define S (+String uint8 'utf8))
 (check-equal? (send S decode stream) "BC")
 (define os (+EncodeStream))
 (send S encode os "Mike")
 (check-equal? (send os dump) #"\4Mike")
 (check-equal? (send (+String) size "foobar") 6))