#lang reader (submod "racket.rkt" reader)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

(define-subclass xenomorph-base% (Enum type [options empty])

  (define/augment (decode stream . _)
    (define index (send type decode stream))
    (or (list-ref options index) index))

  (define/augment (size . _) (send type size))

  (define/augment (encode stream val [ctx #f])
    (define index (index-of options val))
    (unless index
      (raise-argument-error 'Enum:encode "valid option" val))
    (send type encode stream index)))

