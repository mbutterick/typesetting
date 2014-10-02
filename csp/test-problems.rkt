#lang racket
(require "main.rkt")
(require rackunit)

(define-simple-check (check-hash-items h1 h2)
  (for/and ([(k1 v1) (in-hash h1)])
    (equal? (hash-ref h2 k1) v1)))

;; ABC problem:
;; what is the minimum value of

;;       ABC
;;     -------
;;      A+B+C


(define abc-problem (new Problem))
(send abc-problem addVariables '("a" "b" "c") (range 1 10))
(define (test-solution s) (let ([a (hash-ref s "a")]
                                [b (hash-ref s "b")]
                                [c (hash-ref s "c")])
                            (/ (+ (* 100 a) (* 10 b) c) (+ a b c))))

(check-hash-items (argmin test-solution (send abc-problem getSolutions)) 
                  #hash(("c" . 9) ("b" . 9) ("a" . 1)))


;; quarter problem:
;; 26 coins, dollars and quarters
;; that add up to $17.

(define quarter-problem (new Problem))
(send quarter-problem addVariables '("dollars" "quarters") (range 1 27))
(send quarter-problem addConstraint (λ(d q) (= 17 (+ d (* 0.25 q)))) '("dollars" "quarters"))
(send quarter-problem addConstraint (λ(d q) (= 26 (+ d q))) '("dollars" "quarters"))
(check-hash-items (send quarter-problem getSolution) '#hash(("dollars" . 14) ("quarters" . 12)))

;; coin problem 2
#|
A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?
|#

(define nickel-problem (new Problem))
(send nickel-problem addVariables '(nickels dimes quarters) (range 1 34))
(send nickel-problem addConstraint (λ(n d q) (= 33 (+ n d q))) '(nickels dimes quarters))
(send nickel-problem addConstraint (λ(n d q) (= 3.30 (+ (* 0.05 n) (* 0.1 d) (* 0.25 q)))) '(nickels dimes quarters))
(send nickel-problem addConstraint (λ(n q) (= n (* 3 q))) '(nickels quarters))
(send nickel-problem addConstraint (λ(d n) (= n (* 2 d))) '(dimes nickels))
(check-hash-items (send nickel-problem getSolution) #hash((nickels . 18) (quarters . 6) (dimes . 9)))

;; word math
#|
# Assign equal values to equal letters, and different values to
# different letters, in a way that satisfies the following sum:
#
#    TWO
#  + TWO
#  -----
#   FOUR
|#


(define two-four-problem (new Problem))
(send two-four-problem addVariables '(t w o f u r) (range 10))
(send two-four-problem addConstraint (new AllDifferentConstraint))
(send two-four-problem addConstraint (λ(t w o) (> (word-value t w o) 99)) '(t w o))
(send two-four-problem addConstraint (λ(f o u r) (> (word-value f o u r) 999)) '(f o u r))
(send two-four-problem addConstraint 
      (λ (t w o f u r)
        (let ([two (word-value t w o)]
              [four (word-value f o u r)])
          ((two . + . two) . = . four))) '(t w o f u r))
(check-equal? (length (send two-four-problem getSolutions)) 7)
(send two-four-problem addConstraint (λ(r) (= r 0)) '(r))
(check-hash-items (send two-four-problem getSolution) #hash((o . 5) (w . 6) (u . 3) (f . 1) (r . 0) (t . 7)))


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

(define xsum-problem (new Problem))
(send xsum-problem addVariables '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 10))
(send xsum-problem addConstraint (λ (l1 l2 l3 l4 x) 
                                   (and (< l1 l2 l3 l4)
                                        (= 27 (+ l1 l2 l3 l4 x)))) '(l1 l2 l3 l4 x))
(send xsum-problem addConstraint (λ (r1 r2 r3 r4 x) 
                                   (and (< r1 r2 r3 r4)
                                        (= 27 (+ r1 r2 r3 r4 x)))) '(r1 r2 r3 r4 x))
(send xsum-problem addConstraint (new AllDifferentConstraint))
(check-equal? (length (send xsum-problem getSolutions)) 8)



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

(define sm-problem (new Problem))
(send sm-problem addVariables '(s e n d m o r y) (range 10))
(send sm-problem addConstraint (λ(x) (> x 0)) '(s))
(send sm-problem addConstraint (λ(x) (> x 0)) '(m))
(send sm-problem addConstraint (λ(d e y) (= (modulo (+ d e) 10) y)) '(d e y))
(send sm-problem addConstraint (λ(n d r e y)
                                 (= (modulo (+ (word-value n d) (word-value r e)) 100)
                                    (word-value e y))) '(n d r e y))
(send sm-problem addConstraint (λ(e n d o r y)
                                 (= (modulo (+ (word-value e n d) (word-value o r e)) 1000) (word-value n e y))) '(e n d o r y))
(send sm-problem addConstraint (λ(s e n d m o r y) (=
                                                    (+ (word-value s e n d)
                                                       (word-value m o r e))
                                                    (word-value m o n e y))) '(s e n d m o r y))
(send sm-problem addConstraint (new AllDifferentConstraint))

(check-hash-items (send sm-problem getSolution) '#hash((m . 1) (e . 5) (r . 8) (n . 6) (y . 2) (o . 0) (d . 7) (s . 9)))


;; queens problem
;; place queens on chessboard so they do not intersect

(define qp (new Problem))
(define cols (range 8))
(define rows (range 8))
(send qp addVariables cols rows)
(for* ([col1 (in-list cols)] [col2 (in-list cols)] #:when (< col1 col2))
  (send qp addConstraint (λ(row1 row2 [col1 col1][col2 col2])
                           (and 
                            ;; test if two cells are on a diagonal
                            (not (= (abs (- row1 row2)) (abs (- col1 col2))))
                            ;; test if two cells are in same row
                            (not (= row1 row2)))) (list col1 col2))) 
(check-equal? (length (send qp getSolutions)) 92)