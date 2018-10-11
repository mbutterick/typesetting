#lang at-exp racket
(require "csp.rkt" rackunit)

(define demo (make-csp))
(add-vars! demo '(t w) (range 7))
(add-var! demo 'o '(2 6 7))

(define (sum-three t w o) (= 3 (+ t w o)))
(add-constraint! demo sum-three '(t w o))
(add-constraint! demo alldiff '(t w o))
(add-constraint! demo < '(t w o))

(check-equal? (solve demo) ($csp (list ($var 'o '(2)) ($var 'w '(1)) ($var 't '(0))) '()))


;; TWO + TWO = FOUR
(define ttf (make-csp))
(add-vars! ttf '(t w o f u r) (range 10))

(define (word-value . xs)
  (let ([xs (reverse xs)])
    (for/sum ([i (in-range (length xs))])
      (* (list-ref xs i) (expt 10 i)))))

(add-constraint! ttf alldiff '(t w o f u r))
(add-constraint! ttf (λ (t w o f u r) (= (+ (word-value t w o) (word-value t w o))
                                         (word-value f o u r))) '(t w o f u r))
(add-constraint! ttf positive? '(t))
(add-constraint! ttf positive? '(f))

(define ttf-solution (solve ttf)) 
(check-equal? ttf-solution
              ($csp
               (list
                ($var 'r '(0))
                ($var 'u '(3))
                ($var 'f '(1))
                ($var 'o '(5))
                ($var 'w '(6))
                ($var 't '(7)))
               '()))

(define (ttf-print csp)
  (format "~a~a~a + ~a~a~a = ~a~a~a~a" ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 'f) ($csp-ref csp 'o) ($csp-ref csp 'u) ($csp-ref csp 'r)))

(check-equal? (solve ttf-solution ttf-print) "765 + 765 = 1530")


;; ABC problem:
;; what is the minimum value of
;;       ABC
;;     -------
;;      A+B+C

(define abc (make-csp))
(add-vars! abc '(a b c) (range 1 10))
(define (solution-score sol)
  (let ([a ($csp-ref sol 'a)]
        [b ($csp-ref sol 'b)]
        [c ($csp-ref sol 'c)])
    (/ (+ (* 100 a) (* 10 b) c) (+ a b c))))


(define abc-sols (solve* abc))
(check-equal? (* 9 9 9) (length abc-sols))
(check-equal?
 (argmin solution-score abc-sols)
 ($csp (list ($var 'c '(9)) ($var 'b '(9)) ($var 'a '(1))) '()))


;; quarter problem:
;; 26 dollars and quarters
;; that add up to $17.

(define quarter-problem (make-csp))
(add-vars! quarter-problem '(dollars quarters) (range 26))
(add-constraint! quarter-problem (λ (d q) (= 26 (+ d q))) '(dollars quarters))
(add-constraint! quarter-problem (λ (d q) (= 17 (+ d (* 0.25 q)))) '(dollars quarters))
(check-equal? (solve quarter-problem)
              ($csp (list ($var 'quarters '(12)) ($var 'dollars '(14))) '()))


;; nickel problem
#|
A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?
|#
(define ndq-problem (make-csp))
(add-vars! ndq-problem '(n d q) (range 33))
(add-constraint! ndq-problem (λ (n d q) (= 33 (+ n d q))) '(n d q))
(add-constraint! ndq-problem (λ (n d q) (= 330 (+ (* n 5) (* d 10) (* q 25)))) '(n d q))
(add-constraint! ndq-problem (λ (n q) (= (* 3 q) n)) '(n q))
(add-constraint! ndq-problem (λ (d n) (= (* 2 d) n)) '(d n))
(check-equal? (solve ndq-problem)
              ($csp (list ($var 'q '(6)) ($var 'd '(9)) ($var 'n '(18))) '()))


;; xsum
#|
# Reorganize the following numbers in a way that each line of
# 5 numbers sum to 27.
#
#       1   6
#        2 7
#         3
#        8 4
#       9   5
#
|#

(define xsum-problem (make-csp))
(add-vars! xsum-problem '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 1 10))
(add-constraint! xsum-problem (λ (l1 l2 l3 l4 x) 
                                (and (< l1 l2 l3 l4)
                                     (= 27 (+ l1 l2 l3 l4 x)))) '(l1 l2 l3 l4 x))
(add-constraint! xsum-problem (λ (r1 r2 r3 r4 x) 
                                (and (< r1 r2 r3 r4)
                                     (= 27 (+ r1 r2 r3 r4 x)))) '(r1 r2 r3 r4 x))
(add-constraint! xsum-problem alldiff '(l1 l2 l3 l4 r1 r2 r3 r4 x))

;; todo: too slow
#;(check-equal? (length (solve* xsum-problem)) 8)


;; send more money problem
#|
# Assign equal values to equal letters, and different values to
# different letters, in a way that satisfies the following sum:
#
#    SEND
#  + MORE
#  ------
#   MONEY
|#

(define smm (make-csp))
(add-vars! smm '(s e n d m o r y) (range 10))
(add-constraint! smm positive? '(s))
(add-constraint! smm positive? '(m))
(add-constraint! smm (λ (s e n d m o r y)
                       (= (+ (word-value s e n d) (word-value m o r e))
                                             (word-value m o n e y))) '(s e n d m o r y))
(add-constraint! smm alldiff '(s e n d m o r y))

;; todo: too slow
;(solve smm)

;; queens problem
;; place queens on chessboard so they do not intersect
(define queens-problem (make-csp))
(define queens '(q0 q1 q2 q3 q4 q5 q6 q7))
(define rows (range 8))
(add-vars! queens-problem queens rows)
(for* ([(qa qa-col) (in-indexed queens)]
       [(qb qb-col) (in-indexed queens)]
       #:when (< qa-col qb-col))
  (add-constraint! queens-problem
                   (λ (qa-row qb-row)
                     (and 
                      (not (= (abs (- qa-row qb-row)) (abs (- qa-col qb-col)))) ; same diagonal?
                      (not (= qa-row qb-row)))) ; same row?
                   (list qa qb)))

(check-equal? 92 (length (solve* queens-problem)))

