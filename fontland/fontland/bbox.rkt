#lang racket/base
(require racket/struct)
(provide (all-defined-out))

(struct bbox (min-x min-y max-x max-y) #:transparent #:mutable)

(define (+bbox [min-x +inf.0] [min-y +inf.0] [max-x -inf.0] [max-y -inf.0])
  (bbox min-x min-y max-x max-y))

(define (bbox-width bb)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-width "bbox" bb))
  (- (bbox-max-x bb) (bbox-min-x bb)))

(define (bbox-height bb)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-height "bbox" bb))
  (- (bbox-max-y bb) (bbox-min-y bb)))

(define (bbox-add-point bb x y)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-add-point "bbox" bb))
  (set-bbox-min-x! bb (min x (bbox-min-x bb)))  
  (set-bbox-min-y! bb (min y (bbox-min-y bb)))
  (set-bbox-max-x! bb (max x (bbox-max-x bb)))
  (set-bbox-max-y! bb (max y (bbox-max-y bb))))

(define (bbox-copy bb)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-copy "bbox" bb))
  (apply +bbox (bbox->list bb)))

(define bbox->list struct->list)
