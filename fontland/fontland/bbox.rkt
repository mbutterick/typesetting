#lang racket/base
(require racket/contract racket/struct)

(provide (struct-out BBox) make-BBox bbox->list)

(struct BBox (minX minY maxX maxY) #:transparent #:mutable)

(define (make-BBox
         ; The minimum X position in the bounding box
         [minX +inf.0]
         ; The minimum Y position in the bounding box
         [minY +inf.0]
         ; The maxmimum X position in the bounding box
         [maxX -inf.0]
         ; The maxmimum Y position in the bounding box
         [maxY -inf.0])
  (BBox minX minY maxX maxY))

;; The width of the bounding box
(define/contract (width bb)
  (BBox? . -> . number?)
  (- (BBox-maxX bb) (BBox-minX bb)))


;; The height of the bounding box
(define/contract (height bb)
  (BBox? . -> . number?)
  (- (BBox-maxY bb) (BBox-minY bb)))


(define/contract (addPoint bb x y)
  (BBox? number? number? . -> . void?)
  (set-BBox-minX! bb (min x (BBox-minX bb)))  
  (set-BBox-minY! bb (min y (BBox-minY bb)))
  (set-BBox-maxX! bb (max x (BBox-maxX bb)))
  (set-BBox-maxY! bb (max y (BBox-maxY bb))))


(define/contract (copy bb)
  (BBox? . -> . BBox?)
  (apply make-BBox (bbox->list bb)))


(define bbox->list struct->list)
