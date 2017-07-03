#lang reader (submod "racket.rkt" reader)
(require "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define-subclass Streamcoder (Optional type [condition #t])

  (define (resolve-condition parent)
    (if (procedure? condition)
        (condition parent)
        condition))
  
  (define/augment (decode stream parent)
    (when (resolve-condition parent)
      (send type decode stream parent)))

  (define/override (size [val #f] [parent #f])
    (if (resolve-condition parent)
        (send type size val parent)
        0))

  (define/augment (encode stream val parent)
    (when (resolve-condition parent)
      (send type encode stream val parent))))

