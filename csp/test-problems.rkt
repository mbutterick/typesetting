#lang at-exp racket
(require "csp.rkt" rackunit)

(define demo (make-csp))
(add-vars! demo '(t w) (range 7))
(add-var! demo 'o '(2 6 7))

(define (sum-three t w o) (= 3 (+ t w o)))
(add-constraint! demo sum-three '(t w o))
(add-pairwise-constraint! demo alldiff= '(t w o))
(add-pairwise-constraint! demo < '(t w o))

(check-equal? (time (solve demo))  ($csp (list ($var 't '(0)) ($var 'w '(1)) ($var 'o '(2))) '()))


;; TWO + TWO = FOUR

(define (word-value . xs)
  (let ([xs (reverse xs)])
    (for/sum ([i (in-range (length xs))])
      (* (list-ref xs i) (expt 10 i)))))

(define ttf (make-csp))
(add-vars! ttf '(t w o f u r) (reverse (range 10)))
(add-pairwise-constraint! ttf alldiff= '(t w o f u r))
(add-constraint! ttf (λ (o r) (= (modulo (+ o o) 10) r)) '(o r))
(add-constraint! ttf (λ (t w o f u r) (= (+ (word-value t w o) (word-value t w o))
                                         (word-value f o u r))) '(t w o f u r))
(add-constraint! ttf positive? '(t))
(add-constraint! ttf positive? '(f))

(define ttf-solution (time (solve ttf))) 
(check-equal? ttf-solution
              ($csp
               (list
                ($var 't '(7))
                ($var 'w '(3))
                ($var 'o '(4))
                ($var 'f '(1))
                ($var 'u '(6))
                ($var 'r '(8)))
               '()))

(define (ttf-print csp)
  (format "~a~a~a + ~a~a~a = ~a~a~a~a" ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 'f) ($csp-ref csp 'o) ($csp-ref csp 'u) ($csp-ref csp 'r)))

(check-equal? (time (solve ttf-solution ttf-print)) "734 + 734 = 1468")


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


(define abc-sols (time (solve* abc)))
(check-equal? (* 9 9 9) (length abc-sols))
(check-equal?
 (argmin solution-score abc-sols)
 ($csp (list ($var 'a '(1)) ($var 'b '(9)) ($var 'c '(9))) '()))


;; quarter problem:
;; 26 dollars and quarters
;; that add up to $17.

(define quarters (make-csp))
(add-vars! quarters '(dollars quarters) (range 26))
(add-constraint! quarters (λ (d q) (= 26 (+ d q))) '(dollars quarters))
(add-constraint! quarters (λ (d q) (= 17 (+ d (* 0.25 q)))) '(dollars quarters))
(check-equal? (time (solve quarters))
              ($csp (list ($var 'dollars '(14)) ($var 'quarters '(12))) '()))


;; nickel problem
#|
A collection of 33 coins, consisting of nickels, dimes, and quarters, has a value of $3.30. If there are three times as many nickels as quarters, and one-half as many dimes as nickels, how many coins of each kind are there?
|#
(define nickels (make-csp))
(add-vars! nickels '(n d q) (range 33))
(add-constraint! nickels (λ (n d q) (= 33 (+ n d q))) '(n d q) 'count-33)
(add-constraint! nickels (λ (n d q) (= 330 (+ (* n 5) (* d 10) (* q 25)))) '(n d q) 'total-3.30)
(add-constraint! nickels (λ (n q) (= (* 3 q) n)) '(n q) 'triple-nickel)
(add-constraint! nickels (λ (d n) (= (* 2 d) n)) '(d n) 'double-nickel)
(check-equal? (time (solve nickels))
              ($csp (list ($var 'n '(18)) ($var 'd '(9)) ($var 'q '(6))) '()))


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

(define xsum (make-csp))
(add-vars! xsum '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 1 10))
(add-pairwise-constraint! xsum < '(l1 l2 l3 l4))
(add-pairwise-constraint! xsum < '(r1 r2 r3 r4))
(add-constraint! xsum (λ (l1 l2 l3 l4 x) (= 27 (+ l1 l2 l3 l4 x))) '(l1 l2 l3 l4 x))
(add-constraint! xsum (λ (r1 r2 r3 r4 x)  (= 27 (+ r1 r2 r3 r4 x))) '(r1 r2 r3 r4 x))
(add-pairwise-constraint! xsum alldiff= '(l1 l2 l3 l4 r1 r2 r3 r4 x))

(check-equal? (length (time (solve* xsum))) 8)


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
(add-vars! smm '(s e n d m o r y) (λ () (reverse (range 10))))
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

;; todo: too slow
;(solve smm)

;; queens problem
;; place queens on chessboard so they do not intersect
(define queens (make-csp))
(define qs (for/list ([q 8]) (string->symbol (format "q~a" q))))
(define rows (range (length qs)))
(add-vars! queens qs rows)
(define (q-col q) (string->number (string-trim (symbol->string q) "q")))
(for* ([qs (in-combinations qs 2)])
  (match-define (list qa qb) qs)
  (match-define (list qa-col qb-col) (map q-col qs))
  (add-constraint! queens
                   (λ (qa-row qb-row)
                     (and 
                      (not (= (abs (- qa-row qb-row)) (abs (- qa-col qb-col)))) ; same diagonal?
                      (not (= qa-row qb-row)))) ; same row?
                   (list qa qb)))

(check-equal? 92 (length (time (solve* queens))))