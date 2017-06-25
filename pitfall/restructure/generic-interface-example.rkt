#lang br
(require data/collection racket/private/generic-methods)

(define countable<%>
  (interface* ()
              ([(generic-property gen:countable)
                (generic-method-table gen:countable
                                      (define (length o)
                                        (send o length)))])))


(define c (class* object% (countable<%>)
            (super-new)
            (define/public (length) 42)))

c

(define o (make-object c))

(length o)