#lang debug racket
(require "hacs.rkt" rackunit)


(check-equal? (first-unassigned-variable ($csp (list ($var 'a (range 3)) ($var 'b (range 3))) null))
              ($var 'a (range 3)))
(check-equal? (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($var 'b (range 3))) null))
              ($var 'b (range 3)))
(check-false (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($avar 'b (range 3))) null)))

(check-equal?
 ;; no forward checking when no constraints
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1)) ($var 'b (range 2))) null) 'a))
 (list ($avar 'a '(1)) ($var 'b '(0 1))))

(check-equal?
 ($csp-vars (forward-check (forward-check ($csp (list ($avar 'a '(1))  ($avar 'b '(0)) ($var 'c '(0 1 2)))
                                                (list ($constraint '(a c) (negate =))
                                                      ($constraint '(b c) (negate =)))) 'a) 'b))
 (list ($avar 'a '(1)) ($avar 'b '(0)) ($cvar 'c '(2) '(b a))))

(check-equal?
 ;; no inconsistency: b≠c not checked when fc is relative to a
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($var 'b (range 2)) ($var 'c '(0)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'a))
 (list ($avar 'a '(1)) ($cvar 'b '(0) '(a)) ($var 'c '(0))))

(check-equal?
 ;; no inconsistency: a≠b not checked when fc ignores a, which is already assigned
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($avar 'b '(1)) ($var 'c (range 2)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'b))
 (list ($avar 'a '(1)) ($avar 'b '(1)) ($cvar 'c '(0) '(b))))

(check-exn $backtrack?
           (λ () ($csp-vars (forward-check ($csp (list ($avar 'a '(1))
                                                       ($var 'b '(1)))
                                                 (list ($constraint '(a b) (negate =)))) 'a))))


(check-equal? ($csp-vars (forward-check ($csp (list ($var 'a '(0))
                                                    ($var 'b (range 3)))
                                              (list ($constraint '(a b) <))) 'a))
              (list ($var 'a '(0)) ($cvar 'b '(1 2) '(a))))

(check-equal?
 (parameterize ([current-inference forward-check])
   (length (solve* ($csp (list ($var 'x (range 3))
                               ($var 'y (range 3))
                               ($var 'z (range 3)))
                         (list ($constraint '(x y) <>)
                               ($constraint '(x z) <>)
                               ($constraint '(y z) <>)))))) 6)

(parameterize ([current-inference forward-check])
  (define vds (for/list ([k '(wa nt nsw q t v sa)])
                ($var k '(red green blue))))
  (define cs (list
              ($constraint '(wa nt) neq?)
              ($constraint '(wa sa) neq?)
              ($constraint '(nt sa) neq?)
              ($constraint '(nt q) neq?)
              ($constraint '(q sa) neq?)
              ($constraint '(q nsw) neq?)
              ($constraint '(nsw sa) neq?)
              ($constraint '(nsw v) neq?)
              ($constraint '(v sa) neq?)))
  (define csp ($csp vds cs))
  (check-equal? (length (solve* csp)) 18))