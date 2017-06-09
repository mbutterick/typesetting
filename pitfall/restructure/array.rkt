#lang restructure/racket
(require "number.rkt" "utils.rkt" "streamcoder.rkt")
(provide RArray)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass RStreamcoder (RArray type [length #f] [lengthType 'count])
          
  (define/augment (decode stream [parent #f])
    (let ([length (if length
                      (resolveLength length stream parent)
                      (send stream length))])
    
      (caseq lengthType
             [(count) (for/list ([i (in-range length)])
                                (send type decode stream this))])))

  (define/public (size) (unfinished))

  (define/augment (encode stream array [parent #f])
    (for ([item (in-list array)])
         (send type encode stream item))))


(test-module
 (require "decodestream.rkt" "encodestream.rkt")
 (define stream (make-object RDecodeStream #"ABCDEFG"))
   
 (define A (make-object RArray uint16be 3))
 (check-equal? (send A decode stream) '(16706 17220 17734))
 (define os (make-object REncodeStream))
 (send A encode os '(16706 17220 17734))
 (check-equal? (send os dump) #"ABCDEF"))