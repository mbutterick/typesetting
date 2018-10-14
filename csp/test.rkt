#lang at-exp racket
(require "csp.rkt" rackunit)


(define c (make-csp '((a (2 3)) (b (12 14 16)) (c (2 5)))
                    (list ($constraint '(a c) alldiff=)
                          ($constraint '(b c) (λ (b c) (zero? (modulo b c)))))))

(solve c)