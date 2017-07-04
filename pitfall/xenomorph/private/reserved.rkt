#lang reader (submod "racket.rkt" reader)
(require "stream.rkt" "utils.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define-subclass Streamcoder (Reserved type [count 1])

  (define/augment (decode stream parent)
    (send stream pos (+ (· stream pos) (size #f parent)))
    (void))

  (define/augment (size [val #f] [parent #f])
    (* (send type size) (resolve-length count #f parent)))

  (define/augment (encode stream val [parent #f])
    (send stream fill 0 (size val parent))))

