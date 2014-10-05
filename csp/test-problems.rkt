#lang racket
(require "main.rkt" "test-classes.rkt")
(require rackunit)


;; ABC problem:
;; what is the minimum value of

;;       ABC
;;     -------
;;      A+B+C


(define abc-problem (new problem%))
(send abc-problem add-variables '("a" "b" "c") (range 1 10))
(define (test-solution s) (let ([a (hash-ref s "a")]
                                [b (hash-ref s "b")]
                                [c (hash-ref s "c")])
                            (/ (+ (* 100 a) (* 10 b) c) (+ a b c))))

(check-hash-items (argmin test-solution (send abc-problem get-solutions)) 
                  #hash(("c" . 9) ("b" . 9) ("a" . 1)))


;; quarter problem:
;; 26 coins, dollars and quarters
;; that add up to $17.

(define quarter-problem (new problem%))
(send quarter-problem add-variables '("dollars" "quarters") (range 1 27))
(send quarter-problem add-constraint (λ(d q) (= 17 (+ d (* 0.25 q)))) '("dollars" "quarters"))
(send quarter-problem add-constraint (λ(d q) (= 26 (+ d q))) '("dollars" "quarters"))
(check-hash-items (send quarter-problem get-solution) '#hash(("dollars" . 14) ("quarters" . 12)))

;; coin problem 2
#|
A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?
|#

(define nickel-problem (new problem%))
(send nickel-problem add-variables '(nickels dimes quarters) (range 1 34))
(send nickel-problem add-constraint (λ(n d q) (= 33 (+ n d q))) '(nickels dimes quarters))
(send nickel-problem add-constraint (λ(n d q) (= 3.30 (+ (* 0.05 n) (* 0.1 d) (* 0.25 q)))) '(nickels dimes quarters))
(send nickel-problem add-constraint (λ(n q) (= n (* 3 q))) '(nickels quarters))
(send nickel-problem add-constraint (λ(d n) (= n (* 2 d))) '(dimes nickels))
(check-hash-items (send nickel-problem get-solution) #hash((nickels . 18) (quarters . 6) (dimes . 9)))

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


(define two-four-problem (new problem%))
(send two-four-problem add-variables '(t w o f u r) (range 10))
(send two-four-problem add-constraint (new all-different-constraint%))
(send two-four-problem add-constraint (λ(t w o) (> (word-value t w o) 99)) '(t w o))
(send two-four-problem add-constraint (λ(f o u r) (> (word-value f o u r) 999)) '(f o u r))
(send two-four-problem add-constraint 
      (λ (t w o f u r)
        (let ([two (word-value t w o)]
              [four (word-value f o u r)])
          ((two . + . two) . = . four))) '(t w o f u r))
(check-equal? (length (send two-four-problem get-solutions)) 7)
(send two-four-problem add-constraint (λ(r) (= r 0)) '(r))
(check-hash-items (send two-four-problem get-solution) #hash((o . 5) (w . 6) (u . 3) (f . 1) (r . 0) (t . 7)))


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

(define xsum-problem (new problem%))
(send xsum-problem add-variables '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 10))
(send xsum-problem add-constraint (λ (l1 l2 l3 l4 x) 
                                   (and (< l1 l2 l3 l4)
                                        (= 27 (+ l1 l2 l3 l4 x)))) '(l1 l2 l3 l4 x))
(send xsum-problem add-constraint (λ (r1 r2 r3 r4 x) 
                                   (and (< r1 r2 r3 r4)
                                        (= 27 (+ r1 r2 r3 r4 x)))) '(r1 r2 r3 r4 x))
(send xsum-problem add-constraint (new all-different-constraint%))
(check-equal? (length (send xsum-problem get-solutions)) 8)



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

(define sm-problem (new problem%))
(send sm-problem add-variables '(s e n d m o r y) (range 10))
(send sm-problem add-constraint (λ(x) (> x 0)) '(s))
(send sm-problem add-constraint (λ(x) (> x 0)) '(m))
(send sm-problem add-constraint (λ(d e y) (= (modulo (+ d e) 10) y)) '(d e y))
(send sm-problem add-constraint (λ(n d r e y)
                                 (= (modulo (+ (word-value n d) (word-value r e)) 100)
                                    (word-value e y))) '(n d r e y))
(send sm-problem add-constraint (λ(e n d o r y)
                                 (= (modulo (+ (word-value e n d) (word-value o r e)) 1000) (word-value n e y))) '(e n d o r y))
(send sm-problem add-constraint (λ(s e n d m o r y) (=
                                                    (+ (word-value s e n d)
                                                       (word-value m o r e))
                                                    (word-value m o n e y))) '(s e n d m o r y))
(send sm-problem add-constraint (new all-different-constraint%))

(check-hash-items (send sm-problem get-solution) '#hash((m . 1) (e . 5) (r . 8) (n . 6) (y . 2) (o . 0) (d . 7) (s . 9)))


;; queens problem
;; place queens on chessboard so they do not intersect

(define queens-problem (new problem%))
(define cols (range 8))
(define rows (range 8))
(send queens-problem add-variables cols rows)
(for* ([col1 (in-list cols)] [col2 (in-list cols)] #:when (< col1 col2))
  (send queens-problem add-constraint (λ(row1 row2 [col1 col1][col2 col2])
                           (and 
                            ;; test if two cells are on a diagonal
                            (not (= (abs (- row1 row2)) (abs (- col1 col2))))
                            ;; test if two cells are in same row
                            (not (= row1 row2)))) (list col1 col2))) 
(check-equal? (length (send queens-problem get-solutions)) 92)

(module+ main
  (displayln "Tests passed"))