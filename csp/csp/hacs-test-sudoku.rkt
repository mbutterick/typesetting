#lang debug br
(require sugar/debug "hacs.rkt")

(define (make-base-sudoku)
  (define sudoku (make-csp))

  (define cells (range 81))
  (add-vars! sudoku cells (range 1 10))

  (for ([i 9])
    (define row-cells (filter (λ (cell) (= (quotient cell 9) i)) cells))
    (add-all-diff-constraint! sudoku row-cells)
    
    (define col-cells (filter (λ (cell) (= (remainder cell 9) i)) cells))
    (add-all-diff-constraint! sudoku col-cells))

  (define box-starts '(0 3 6 27 30 33 54 57 60))
  (define box-offsets '(0 1 2 9 10 11 18 19 20))
  (for ([start box-starts])
    (add-all-diff-constraint! sudoku (map (curry + start) box-offsets)))
  
  sudoku)

(define (make-sudoku-board . strs)
  (define sudoku (make-base-sudoku))
  (define vals (for*/list ([str (in-list strs)]
                           [c (in-string str)]
                           #:unless (memv c '(#\- #\|)))
                 (string->number (string c))))
  (for ([(val vidx) (in-indexed vals)]
        #:when val)
    (add-constraint! sudoku (curry = val) (list vidx)))
  sudoku)

(require racket/sequence)
(define (print-grid sol)
  (displayln (string-join (map ~a (for/list ([row (in-slice 9 (csp->assocs sol))])
                                    (map cdr row))) "\n")))

;; http://jeapostrophe.github.io/2013-10-23-sudoku-post.html

(define b1
  (make-sudoku-board
   "53 | 7 |   "
   "6  |195|   "
   " 98|   | 6 "
   "-----------"
   "8  | 6 |  3"
   "4  |8 3|  1"
   "7  | 2 |  6"
   "-----------"
   " 6 |   |28 "
   "   |419|  5"
   "   | 8 | 79"))

;; "Hard" example
(define b2
  (make-sudoku-board
   " 7 | 2 |  5"
   "  9| 87|  3"
   " 6 |   | 4 "
   "-----------"
   "   | 6 | 17"
   "9 4|   |8 6"
   "71 | 5 |   "
   "-----------"
   " 9 |   | 8 "
   "5  |21 |4  "
   "4  | 9 | 6 "))

;; "Evil" example
(define b3
  (make-sudoku-board
   "  8|   | 45"
   "   | 8 |9  "
   "  2|4  |   "
   "-----------"
   "5  |  1|76 "
   " 1 | 7 | 8 "
   " 79|5  |  1"
   "-----------"
   "   |  7|4  "
   "  7| 6 |   "
   "65 |   |3  "))

(current-inference forward-check)
(current-select-variable mrv-degree-hybrid)
(current-order-values shuffle)
(current-node-consistency #t)
(current-arity-reduction #t)
(define trials 5)
(time-avg trials (void (solve b1)))
(print-debug-info)
(time-avg trials (void (solve b2)))
(print-debug-info)
(time-avg trials (void (solve b3)))
(print-debug-info)


(define (euler-value sol)
  (match sol
    [(list (cons 0 h) (cons 1 t) (cons 2 d) _ ...) 
     (+ (* 100 h) (* 10 t) d)]))


(require rackunit)
(check-equal? (euler-value (solve b1)) 534)
(check-equal? (euler-value (solve b2)) 378)
(check-equal? (euler-value (solve b3)) 938)

;; https://projecteuler.net/problem=96
;; answer 24702
(define (do-euler)
  (define bstrs
    (for/list ([puz (in-slice 10 (string-split (port->string (open-input-file "euler-sudoku-grids.txt")) "\n"))])
      (map (λ (str) (string-replace str "0" " ")) (cdr puz))))
  (for/sum ([bstr bstrs])
    (euler-value (solve (apply make-sudoku-board bstr)))))
