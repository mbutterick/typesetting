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
(add-vars! ttf '(t w o f u r c10 c100) (range 10))

(add-constraint! ttf alldiff '(t w o f u r))
(define (adder arg1 arg2 ones-digit tens-digit)
  (= (+ arg1 arg2) (+ (* 10 tens-digit) ones-digit)))
(add-constraint! ttf adder '(o o r c10))
(add-constraint! ttf adder '(w w u c100))
(add-constraint! ttf adder '(t t o f))
(add-constraint! ttf positive? '(f))

(define ttf-solution (solve ttf)) 
(check-equal? ttf-solution
              ($csp
               (list
                ($var 'c100 '(0))
                ($var 'c10 '(0))
                ($var 'r '(8))
                ($var 'u '(6))
                ($var 'f '(1))
                ($var 'o '(4))
                ($var 'w '(3))
                ($var 't '(7)))
               '()))

(define (ttf-print csp)
  (format "~a~a~a + ~a~a~a = ~a~a~a~a" ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 't) ($csp-ref csp 'w) ($csp-ref csp 'o) ($csp-ref csp 'f) ($csp-ref csp 'o) ($csp-ref csp 'u) ($csp-ref csp 'r)))

(check-equal? (solve ttf-solution ttf-print) "734 + 734 = 1468")


;; ABC problem:
;; what is the minimum value of
;;       ABC
;;     -------
;;      A+B+C

(define abc (make-csp))
(add-vars! abc '(a b c) (range 1 10))
(define (solution-score abc)
  (let ([a ($csp-ref abc 'a)]
        [b ($csp-ref abc 'b)]
        [c ($csp-ref abc 'c)])
    (/ (+ (* 100 a) (* 10 b) c) (+ a b c))))

(check-equal?
 (argmin solution-score (solve* abc))
 ($csp (list ($var 'c '(9)) ($var 'b '(9)) ($var 'a '(1))) '()))