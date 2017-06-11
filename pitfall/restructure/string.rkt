#lang restructure/racket
(require "number.rkt" "utils.rkt" "streamcoder.rkt")
(provide RString)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define-subclass RStreamcoder (RString [length #f] [encoding 'ascii])
  (field [_codec (caseq encoding
                        [(latin-1 ascii) (cons string->bytes/latin-1 bytes->string/latin-1)]
                        [(utf-8 utf8)(cons string->bytes/utf-8 bytes->string/utf-8)])])
         
  (define/augment (decode stream [parent #f])
    (define count (if length
                      (resolveLength length stream parent)
                      (send stream length)))
    (define bytes (send stream read count))
    ((cdr _codec) bytes))

  (define/augment (encode stream val [parent #f])
    (define bytes ((car _codec) (format "~a" val)))
    
    (when (is-a? length Number) ;; length-prefixed string
      (send length encode stream (bytes-length bytes)))
    
    (send stream write bytes))

  (define/override (size) (unfinished)))


(test-module
 (require "decodestream.rkt" "encodestream.rkt")
 (define stream (make-object RDecodeStream #"\2BCDEF"))
 (define S (make-object RString uint8 'utf8))
 (check-equal? (send S decode stream) "BC")
 (define os (make-object REncodeStream))
 (send S encode os "Mike")
 (check-equal? (send os dump) #"\4Mike"))