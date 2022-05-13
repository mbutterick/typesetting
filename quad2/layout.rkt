#lang debug racket/base
(require racket/contract
         "pipeline.rkt"
         "quad.rkt")
(provide layout)

(define/contract (posn-add p0 p1)
  ($point? $size? . -> . $point?)
  ($point (+ ($point-x p0) ($size-width p1)) (+ ($point-y p0) ($size-height p1))))

(define/contract (size char)
  (quad? . -> . $size?)
  ($size 1 1))

(define/contract (advance char)
  (quad? . -> . $size?)
  ($size 1 0))

(define (min-x rect) ($point-x ($rect-origin rect)))
(define (width rect) ($size-width ($rect-size rect)))
(define (max-x rect) (+ (min-x rect) (width rect)))
(define (min-y rect) ($point-y ($rect-origin rect)))
(define (height rect) ($size-height ($rect-size rect)))
(define (max-y rect) (+ (min-y rect) (height rect)))

(define/contract (rect-contains-point? rect pt)
  ($rect? $point? . -> . boolean?)
  ;; https://developer.apple.com/documentation/foundation/1391317-nspointinrect/
  ;; "Point-in-rectangle functions generally assume that the “upper” and “left” edges of a rectangle are inside the rectangle boundaries, while the “lower” and “right” edges are outside the boundaries. This method treats the “upper” and “left” edges of the rectangle as the ones containing the origin of the rectangle."
  (and
   ;; IOW the point (min-x, min-y) is inside rect,
   (<= (min-x rect) ($point-x pt))
   (<= (min-y rect) ($point-y pt))
   ;; and the point (max-x, max-y) is not
   (< ($point-x pt) (max-x rect))
   (< ($point-y pt) (max-y rect))))

(define/contract (rect-contains-rect? outer-aRect inner-bRect)
  ($rect? $rect? . -> . boolean?)
  ;; https://developer.apple.com/documentation/foundation/1391177-nscontainsrect
  ;; "true if aRect completely encloses bRect. For this condition to be true, bRect cannot be empty, and must not extend beyond aRect in any direction."
  ;; thus a rect always contains itself.
  ;; TODO: why can't bRect be empty?
  (and (<= (min-x outer-aRect) (min-x inner-bRect)) (<= (max-x inner-bRect) (max-x outer-aRect))
       (<= (min-y outer-aRect) (min-y inner-bRect)) (<= (max-y inner-bRect) (max-y outer-aRect))))

(define-pass (layout qs)
  #:pre (list-of has-no-position?)
  #:post (list-of has-position?)
  (define frame ($rect ($point 0 0) ($size (current-wrap-width) 30)))
  (define (quad-fits? q posn)
    (rect-contains-rect? frame ($rect posn (size q))))
  (for/fold ([posn0 ($point 0 0)]
             #:result qs)
            ([q (in-list qs)])
    (define first-posn-on-next-line ($point 0 (add1 ($point-y posn0))))
    (define other-possible-posns (list first-posn-on-next-line))
    (define posn1 (for/first ([posn (in-list (cons posn0 other-possible-posns))]
                              #:when (quad-fits? q posn))
                             posn))
    (unless posn1
      (error 'no-posn-that-fits))
    (set-quad-posn! q posn1)
    (posn-add posn1 (advance q))))