#lang debug racket
(require sugar/debug "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv)
(current-order-values shuffle)
(current-random #true)

;; queens problem
;; place queens on chessboard so they do not intersect

(define board-size 8)

(define queens (make-csp))
(define qs (for/list ([q board-size]) (string->symbol (format "q~a" q))))
(define rows (range (length qs)))
(add-vars! queens qs rows)
(define (q-col q) (string->number (string-trim (symbol->string q) "q")))
(for* ([qs (in-combinations qs 2)])
  (match-define (list qa qb) qs)
  (match-define (list qa-col qb-col) (map q-col qs))
  (add-constraint! queens
                   (λ (qa-row qb-row)
                     (not (= (abs (- qa-row qb-row)) (abs (- (q-col qa) (q-col qb)))))) ; same diag?
                   (list qa qb))
  (add-constraint! queens (negate =) (list qa qb)))

(time-avg 10 (solve queens))
(parameterize ([current-solver min-conflicts-solver])
  (time-named (solve queens)))