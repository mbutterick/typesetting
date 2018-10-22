#lang br
(require "aima.rkt" sugar/debug)


;; queens problem
;; place queens on chessboard so they do not intersect
(define qs (for/list ([q 8]) (string->symbol (format "q~a" q))))
(define rows (range (length qs)))
(define vds (for/list ([q qs])
                      ($vd q (range (length qs)))))
(define (q-col q) (string->number (string-trim (symbol->string q) "q")))
(define cs (for*/list ([qs (in-combinations qs 2)])
                      (match-define (list qa qb) qs)
                      (match-define (list qa-col qb-col) (map q-col qs))
                      ($constraint
                       (list qa qb)
                       (λ (qa-row qb-row)
                         (and 
                          (not (= (abs (- qa-row qb-row)) (abs (- qa-col qb-col)))) ; same diagonal?
                          (not (= qa-row qb-row)))))))

(define queens (make-csp vds cs))

(current-solver min-conflicts)
(time-named (solve queens))