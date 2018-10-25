#lang debug br
(require sugar/debug "hacs.rkt")

(define names (for/list ([i (in-range 81)])
                        (string->symbol (format "c~a" i))))

(define (make-sudoku)
  (define sudoku (make-csp))
  (add-vars! sudoku names (range 1 10))

  (define (not= . xs) (= (length xs) (length (remove-duplicates xs =)))) 

  (for ([i (in-range 9)])
       (define row-cells (for/list ([(name idx) (in-indexed names)]
                                    #:when (= (quotient idx 9) i))
                                   name))
       (add-pairwise-constraint! sudoku not= row-cells)
       (define col-cells (for/list ([(name idx) (in-indexed names)]
                                    #:when (= (remainder idx 9) i))
                                   name))
       (add-pairwise-constraint! sudoku not= col-cells))
  
  (for ([i '(0 3 6 27 30 33 54 57 60)])
       (define box-cells (for/list ([j '(0 1 2 9 10 11 18 19 20)])
                                   (string->symbol (format "c~a" (+ i j)))))
       (add-pairwise-constraint! sudoku not= box-cells))
  
  sudoku)

(require racket/sequence)
(define (print-grid sol)
  (displayln (string-join (map ~a (for/list ([row (in-slice 9 (csp->assocs sol))])
                                 (map cdr row))) "\n")))

(define (board . strs)
  (define sudoku (make-sudoku))
  (define vals
    (for*/list ([str strs]
                [c (in-port read-char (open-input-string str))]
                #:unless (memv c '(#\- #\|)))
               (match (string c)
                 [(? string->number num) (string->number num)]
                 [else #f])))
  (for ([name names]
        [val vals]
        #:when val)
       (add-constraint! sudoku (curry = val) (list name) (string->symbol (format "is-~a" val))))
  sudoku)

;; http://jeapostrophe.github.io/2013-10-23-sudoku-post.html

(define b1
  (board
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
  (board
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
  (board
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
(current-random #true)
(current-node-consistency #t)
(current-arity-reduction #t)
(time-avg 10 (solve b1 #:finish-proc print-grid))
(time-avg 10 (solve b2 #:finish-proc print-grid))
(time-avg 10 (solve b3 #:finish-proc print-grid))