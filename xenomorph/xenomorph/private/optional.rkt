#lang racket/base
(require "racket.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define-subclass xenomorph-base% (Optional type [condition #t])

  (define (resolve-condition parent)
    (if (procedure? condition)
        (condition parent)
        condition))
  
  (define/augment (decode stream parent)
    (when (resolve-condition parent)
      (send type decode stream parent)))

  (define/augment (size val parent)
    (when (resolve-condition parent)
      (send type size val parent)))

  (define/augment (encode stream val parent)
    (when (resolve-condition parent)
      (send type encode stream val parent))))

