#lang racket/base
(require "racket.rkt")
(require "utils.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define-subclass xenomorph-base% (Reserved type [count 1])

  (define/augment (decode port parent)
    (pos port (+ (pos port) (size #f parent)))
    (void))

  (define/augment (size [val #f] [parent #f])
    (* (send type size) (resolve-length count #f parent)))

  (define/augment (encode port val [parent #f])
    (make-bytes (size val parent) 0)))

