#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass RBase (RStruct [fields (mhash)])

  (define/override (decode stream parent [length 0])
    (unfinished))

  (define/override (encode stream val parent)
    (unfinished))
  )


(make-object RStruct (mhash 'foo "bar"))