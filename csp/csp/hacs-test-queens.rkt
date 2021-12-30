#lang debug racket
(require sugar/debug "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv)
(current-order-values shuffle)

;; queens problem
;; place queens on chessboard so they do not intersect

(define board-size 10)

(define queens (make-csp))
(define qs (range board-size))
(define rows (range (length qs)))
(add-vars! queens qs rows)
(for* ([qs (in-combinations qs 2)])
  (match-define (list qa qb) qs)
  (add-constraint! queens
                   (λ (qa-row qb-row)
                     (not (= (abs (- qa-row qb-row)) (abs (- qa qb))))) ; same diag?
                   (list qa qb)))
(add-all-diff-constraint! queens #:proc eq?)

(define (sol->string sol)
  (define assocs (csp->assocs sol))
  (displayln (string-join (for/list ([q (in-list (sort assocs < #:key car))])
                 (apply string (add-between (for/list ([idx (in-range board-size)])
                                              (if (= idx (cdr q)) #\@ #\·)) #\space))) "\n"))
  assocs)

(current-thread-count 4)
(parameterize ([current-solver min-conflicts-solver])
  (time (solve queens #:finish-proc sol->string)))

