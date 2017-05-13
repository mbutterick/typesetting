#lang racket/base
(require racket/class racket/match racket/string racket/format)
(provide vector-mixin)
(require "helper.rkt")

(define default-ctm-value '(1 0 0 1 0 0))

(define (vector-mixin %)
  (class %
    (super-new)
    (field [(@_ctm _ctm) default-ctm-value]
           [(@_ctmStack _ctmStack) null])
    
    (define/public (initVector)
      (set! @_ctm default-ctm-value)
      (set! @_ctmStack null))

    (define/public (save)
      (push! @_ctmStack @_ctm)
      (send this addContent "q"))

    (define/public (restore)
      (set! @_ctm (if (pair? @_ctmStack) (pop! @_ctmStack) default-ctm-value))
      (send this addContent "Q"))
    
    (define/public (transform m11 m12 m21 m22 dx dy #:debug [debug #f])
      ;; keep track of the current transformation matrix
      (match-define (list m0 m1 m2 m3 m4 m5) @_ctm)
      (set! @_ctm (list (+ (* m0 m11) (* m2 m12))
                        (+ (* m1 m11) (* m3 m12))
                        (+ (* m0 m21) (* m2 m22))
                        (+ (* m1 m21) (* m3 m22))
                        (+ (* m0 dx) (* m2 dy) m4)
                        (+ (* m1 dx) (* m3 dy) m5)))
      (define values (string-join (map number (list m11 m12 m21 m22 dx dy)) " "))
      (define result (format "~a cm" values))
      (if debug
          result
          (send this addContent result)))

    (define/public (moveTo x y)
      (send this addContent (format "~a ~a m" x y)))

    (define/public (lineTo x y)
      (send this addContent (format "~a ~a l" x y)))

    (public [@_windingRule _windingRule])
    (define (@_windingRule rule)
      (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule))
          "*"
          ""))

    (define/public (fill color [rule #f])
      (when (regexp-match #rx"^(even-?odd)|(non-?zero)$" color)
        (set! rule color)
        (set! color #f))
      (when color (send this fillColor color)) ;; fillColor method is from color mixin
      (send this addContent (format "f~a" (@_windingRule rule))))

    ))

(module+ test
  (require rackunit)
  (define v (new (vector-mixin object%)))
  (check-equal? (· v _ctm) default-ctm-value)
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(1 2 3 4 5 6))
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(7 10 15 22 28 40))
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(37 54 81 118 153 222)))