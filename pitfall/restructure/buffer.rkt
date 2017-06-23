#lang restructure/racket
(require "number.rkt" (prefix-in utils- "utils.rkt"))
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

#|
A Buffer is a container object for any data object that supports random access
|#


(define-subclass RestructureBase (Buffer [length_ #xffff])
  
  (define/override (decode stream [parent #f])
    (define length__ (utils-resolveLength length_ stream parent))
    (send stream readBuffer length__))

  (define/override (size [val #f] [parent #f])
    (when val (unless (bytes? val)
                (raise-argument-error 'Buffer:size "bytes" val)))
    (if val
        (bytes-length val)
        (utils-resolveLength length_ val parent)))

  (define/override (encode stream buf [parent #f])
    (unless (and (list? buf) (andmap byte? buf))
      (raise-argument-error 'Buffer:encode "list of bytes" buf))
    (when (NumberT? length_)
      (send length_ encode stream (length buf))
      (send stream writeBuffer buf))))


(define-subclass Buffer (BufferT))


#;(test-module
   (require "stream.rkt")
   (define stream (+DecodeStream #"\2BCDEF"))
   (define S (+String uint8 'utf8))
   (check-equal? (send S decode stream) "BC")
   (define os (+EncodeStream))
   (send S encode os "Mike")
   (check-equal? (send os dump) #"\4Mike")
   (check-equal? (send (+String) size "foobar") 6))