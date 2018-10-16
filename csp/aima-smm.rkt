#lang br
(require "aima.rkt")

;  SEND
;+ MORE
;------
; MONEY
(define (word-value . xs)
  (for/sum ([(x idx) (in-indexed (reverse xs))])
    (* x (expt 10 idx))))

(define vs '(s e n d m o r y))
(define ds (for/hash ([k vs])
             (values k (range 10))))
(define ns (for*/hash ([v (in-list vs)])
             (values v (remove v vs))))

(define (smm-constraint A a B b)
  (and
   (not (eq? a b))
   (when (eq? A 's) (= 1 a))))

(define csp (make-csp vs ds ns smm-constraint))
(parameterize ([current-select-variable mrv]
               [current-order-values lcv]
               [current-inference mac]
               [current-reset #f])
  (solve csp))
(nassigns csp)
(nchecks csp)