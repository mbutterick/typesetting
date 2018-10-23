#lang debug racket
(require sugar/debug "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv)
(current-order-values shuffle)
(current-random #true)

(define (word-value . xs)
  (for/sum ([(x idx) (in-indexed (reverse xs))])
           (* x (expt 10 idx))))


(define smm (make-csp))

(define vs '(s e n d m o r y))
(add-vars! smm vs (λ () (range 10)))

(add-constraint! smm positive? '(s))
(add-constraint! smm positive? '(m))
(add-constraint! smm (λ (d e y) (= (modulo (+ d e) 10) y)) '(d e y))
(add-constraint! smm (λ (n d r e y)
                       (= (modulo (+ (word-value n d) (word-value r e)) 100)
                          (word-value e y))) '(n d r e y))
(add-constraint! smm (λ (e n d o r y)
                       (= (modulo (+ (word-value e n d) (word-value o r e)) 1000) (word-value n e y))) '(e n d o r y))
(add-constraint! smm (λ (s e n d m o r y)
                       (= (+ (word-value s e n d) (word-value m o r e))
                          (word-value m o n e y))) '(s e n d m o r y))
(add-pairwise-constraint! smm alldiff= '(s e n d m o r y))
(parameterize ([current-select-variable mrv-degree-hybrid]
               [current-node-consistency make-nodes-consistent]) ; todo: why is plain mrv so bad on this problem?
  (time-named (solve smm)))

nassns
nfchecks
nchecks

