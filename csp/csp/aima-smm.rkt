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
(define vds (for/list ([k vs])
              ($vd k (range 10))))

(define (not= x y) (not (= x y)))

(define alldiffs
  (for/list ([pr (in-combinations vs 2)])
    ($constraint pr not=)))

(define smm (make-csp vds (append
                           alldiffs
                           (list
                            ($constraint '(s) positive?)
                            ($constraint '(m) positive?)
                            ($constraint '(d e y) (λ (d e y) (= (modulo (+ d e) 10) y)))
                            ($constraint '(n d r e y) (λ (n d r e y)
                                                        (= (modulo (+ (word-value n d) (word-value r e)) 100)
                                                           (word-value e y))))
                            ($constraint '(e n d o r y) (λ (e n d o r y)
                                                          (= (modulo (+ (word-value e n d) (word-value o r e)) 1000) (word-value n e y))))
                            ($constraint '(s e n d m o r y) (λ (s e n d m o r y)
                       (= (+ (word-value s e n d) (word-value m o r e))
                          (word-value m o n e y))))))))

(parameterize ([current-select-variable mrv]
               [current-order-values lcv]
               [current-inference mac])
  (time (solve smm)))
(nassigns smm)
(nchecks smm)
(reset! smm)