#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(struct $codec (encoder decoder) #:transparent)

(define-subclass Streamcoder (String [strlen #f] [encoding 'ascii])
  (field [codec (caseq encoding
                       [(latin-1 ascii) ($codec string->bytes/latin-1 bytes->string/latin-1)]
                       [(utf-8 utf8) ($codec string->bytes/utf-8 bytes->string/utf-8)])])
         
  (define/augment (decode stream [parent #f])
    (define count (if strlen
                      (resolveLength strlen stream parent)
                      (send stream length)))
    (define bytes (send stream read count))
    (($codec-decoder codec) bytes))

  (define/augment (encode stream val [parent #f])
    (define bytes (($codec-encoder codec) (format "~a" val)))
    
    (when (Number? strlen) ;; length-prefixed string
      (send strlen encode stream (bytes-length bytes)))
    
    (send stream write bytes))

  (define/override (size [str-in #f])
    (define str (or str-in (make-string strlen #\x)))
    (define es (+EncodeStream))
    (encode es str)
    (bytes-length (send es dump))))


(test-module
 (require "stream.rkt")
 (define stream (+DecodeStream #"\2BCDEF"))
 (define S (+String uint8 'utf8))
 (check-equal? (send S decode stream) "BC")
 (define os (+EncodeStream))
 (send S encode os "Mike")
 (check-equal? (send os dump) #"\4Mike")
 (check-equal? (send (+String) size "foobar") 6))