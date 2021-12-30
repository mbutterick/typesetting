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

(define (bbox-add-point! bb x y)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-add-point "bbox" bb))
  (set-bbox-min-x! bb (min x (bbox-min-x bb)))  
  (set-bbox-min-y! bb (min y (bbox-min-y bb)))
  (set-bbox-max-x! bb (max x (bbox-max-x bb)))
  (set-bbox-max-y! bb (max y (bbox-max-y bb)))
  (void))

(define (bbox-copy bb)
  (unless (bbox? bb)
    (raise-argument-error 'bbox-copy "bbox" bb))
  (apply +bbox (bbox->list bb)))

(define bbox->list struct->list)

(module+ test
  (require rackunit)
  (define bb (+bbox 1 2 4 8))
  (check-equal? (bbox-width bb) 3)
  (check-equal? (bbox-height bb) 6)
  (bbox-add-point! bb 0 0)
  (check-equal? (bbox-width bb) 4)
  (check-equal? (bbox-height bb) 8)
  (check-equal? (bbox->list (bbox-copy bb)) (bbox->list bb)))
