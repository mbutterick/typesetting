#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

(define-subclass RestructureBase (Buffer [length_ #xffff])
  (define/augride (decode stream [parent #f])
    (define len (resolveLength length_ stream parent))
    (send stream readBuffer len))

  (define/override (size [val #f] [parent #f])
    (when val (unless (bytes? val)
                (raise-argument-error 'Buffer:size "bytes" val)))
    (if val
        (bytes-length val)
        (resolveLength length_ val parent)))

  (define/override (encode stream buf [parent #f])
    (when (Number? length_)
      (send length_ encode stream (bytes-length buf)))
    (send stream writeBuffer buf)))

(define (bytes->Buffer bstr)
  (define b (+Buffer (bytes-length bstr)))
  (send b decode (+DecodeStream bstr))
  b)


#;(test-module
 (require "stream.rkt")
 (define stream (+DecodeStream #"\2BCDEF"))
 (define S (+String uint8 'utf8))
 (check-equal? (send S decode stream) "BC")
 (define os (+EncodeStream))
 (send S encode os "Mike")
 (check-equal? (send os dump) #"\4Mike")
 (check-equal? (send (+String) size "foobar") 6))