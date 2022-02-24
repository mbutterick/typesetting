#lang racket/base
(provide (all-defined-out))

(struct $point (x y) #:transparent #:mutable)
(struct $size (width height) #:transparent #:mutable)
(struct $rect (origin size) #:transparent #:mutable)

(struct $quad (posn char) #:transparent #:mutable)

(define current-wrap-width (make-parameter 5))