#lang fontkit/racket
(provide BBox bbox->list)

(define-subclass object% (BBox
                          ; The minimum X position in the bounding box
                          [minX +inf.0]
                          ; The minimum Y position in the bounding box
                          [minY +inf.0]
                          ; The maxmimum X position in the bounding box
                          [maxX -inf.0]
                          ; The maxmimum Y position in the bounding box
                          [maxY -inf.0])

  (as-methods
   width
   height
   addPoint
   copy))

;; The width of the bounding box
(define/contract (width this)
  (->m number?)
  (- (· this maxX) (· this minX)))


;; The height of the bounding box
(define/contract (height this)
  (->m number?)
  (- (· this maxY) (· this minY)))


(define/contract (addPoint this x y)
  (number? number? . ->m . void?)
  (set-field! minX this (min x (· this minX)))  
  (set-field! minY this (min y (· this minY)))
  (set-field! maxX this (max x (· this maxX)))
  (set-field! maxY this (max y (· this maxY))))


(define/contract (copy this)
  (->m (is-a?/c BBox))
  (make-object BBox (· this minX)
    (· this minY)
    (· this maxX)
    (· this maxY)))


(define/contract (bbox->list this)
  ((is-a?/c BBox) . -> . (list/c number? number? number? number?))
  (list (· this minX) (· this minY) (· this maxX) (· this maxY)))
