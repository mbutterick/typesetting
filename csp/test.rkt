#lang at-exp racket
(require "csp.rkt" rackunit)

(let ([demo (new-csp)])
  (define digits (range 7))
  (add-var! demo 't digits)
  (add-var! demo 'w digits)
  (add-var! demo 'o '(2 6 7))

  (define (sum-three t w o) (= 3 (+ t w o)))
  (add-constraint! demo sum-three 't 'w 'o)

  (define diff (compose1 not =))
  (add-constraint! demo diff 't 'w)
  (add-constraint! demo diff 'w 'o)
  (add-constraint! demo diff 't 'o)

  (add-constraint! demo < 't 'w)

  (define three-or-less (curryr <= 3))
  (add-constraint! demo three-or-less 't)
  (add-constraint! demo three-or-less 'w)
  (add-constraint! demo three-or-less 'o)
  (check-equal? (solve demo) ($csp (list ($var 'o '(2)) ($var 'w '(1)) ($var 't '(0))) '())))


;; TWO + TWO = FOUR
(define ttf (new-csp))
(define digs (range 10))
(add-var! ttf 't digs)
(add-var! ttf 'w digs)
(add-var! ttf 'o digs)
(add-var! ttf 'f digs)
(add-var! ttf 'u digs)
(add-var! ttf 'r digs)

(add-var! ttf 'c10 digs)
(add-var! ttf 'c100 digs)
(add-var! ttf 'c1000 digs)

(add-constraint! ttf alldiff 't 'w 'o 'f 'u 'r)
(define (adder arg1 arg2 ones-digit tens-digit) (= (+ arg1 arg2) (+ (* 10 tens-digit) ones-digit)))
(add-constraint! ttf adder 'o 'o 'r 'c10)
(add-constraint! ttf adder 'w 'w 'u 'c100)
(add-constraint! ttf adder 't 't 'o 'c1000)
(add-constraint! ttf positive? 'f)
(add-constraint! ttf = 'f 'c1000)


(define ttf-solution (solve ttf)) 
(check-equal? ttf-solution
              ($csp
               (list
                ($var 'c1000 '(1))
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
