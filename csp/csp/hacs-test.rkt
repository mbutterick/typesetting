#lang debug racket
(require "hacs.rkt" rackunit sugar/list sugar/debug)

(current-inference forward-check)
(current-select-variable mrv-degree-hybrid)
(current-order-values shuffle)
(current-random #true)
(current-node-consistency #f)
(current-arity-reduction #t)

(check-equal? (first-unassigned-variable (csp (list (var 'a (range 3)) (var 'b (range 3))) null))
              (var 'a (range 3)))
(check-equal? (first-unassigned-variable (csp (list (avar 'a (range 3)) (var 'b (range 3))) null))
              (var 'b (range 3)))
(check-false (first-unassigned-variable (csp (list (avar 'a (range 3)) (avar 'b (range 3))) null)))

(check-equal?
 ;; no forward checking when no constraints
 (csp-vars (forward-check (csp (list (avar 'a '(1)) (var 'b (range 2))) null) 'a))
 (list (avar 'a '(1)) (var 'b '(0 1))))

(check-equal?
 (csp-vars (forward-check (forward-check (csp (list (avar 'a '(1))  (avar 'b '(0)) (var 'c '(0 1 2)))
                                                (list (constraint '(a c) (negate =))
                                                      (constraint '(b c) (negate =)))) 'a) 'b))
 (list (avar 'a '(1)) (avar 'b '(0)) (cvar 'c '(2) '((b . 0) (a . 1)))))

(check-equal?
 ;; no inconsistency: b≠c not checked when fc is relative to a
 (csp-vars (forward-check (csp (list (avar 'a '(1))  (var 'b (range 2)) (var 'c '(0)))
                                 (list (constraint '(a b) (negate =))
                                       (constraint '(b c) (negate =)))) 'a))
 (list (avar 'a '(1)) (cvar 'b '(0) '((a . 1))) (var 'c '(0))))

(check-equal?
 ;; no inconsistency: a≠b not checked when fc ignores a, which is already assigned
 (csp-vars (forward-check (csp (list (avar 'a '(1))  (avar 'b '(1)) (var 'c (range 2)))
                                 (list (constraint '(a b) (negate =))
                                       (constraint '(b c) (negate =)))) 'b))
 (list (avar 'a '(1)) (avar 'b '(1)) (cvar 'c '(0) '((b . 1)))))

(check-exn backtrack?
           (λ () (csp-vars (forward-check (csp (list (avar 'a '(1))
                                                       (var 'b '(1)))
                                                 (list (constraint '(a b) (negate =)))) 'a))))


(check-equal? (csp-vars (forward-check (csp (list (var 'a '(0))
                                                    (var 'b (range 3)))
                                              (list (constraint '(a b) <))) 'a))
              (list (var 'a '(0)) (cvar 'b '(1 2) '((a . 0)))))

(check-equal?
 (parameterize ([current-inference forward-check])
   (length (solve* (csp (list (var 'x (range 3))
                               (var 'y (range 3))
                               (var 'z (range 3)))
                         (list (constraint '(x y) <>)
                               (constraint '(x z) <>)
                               (constraint '(y z) <>)))))) 6)

(parameterize ([current-inference forward-check])
  (define vds (for/list ([k '(wa nt nsw q t v sa)])
                        (var k '(red green blue))))
  (define cs (list
              (constraint '(wa nt) neq?)
              (constraint '(wa sa) neq?)
              (constraint '(nt sa) neq?)
              (constraint '(nt q) neq?)
              (constraint '(q sa) neq?)
              (constraint '(q nsw) neq?)
              (constraint '(nsw sa) neq?)
              (constraint '(nsw v) neq?)
              (constraint '(v sa) neq?)))
  (define aus (csp vds cs))
  (check-equal? (length (solve* aus)) 18))


(define quarters (make-csp))
(add-vars! quarters '(dollars quarters) (range 26))
(add-constraint! quarters (λ (d q) (= 26 (+ d q))) '(dollars quarters))
(add-constraint! quarters (λ (d q) (= 17 (+ d (* 0.25 q)))) '(dollars quarters))
(check-equal? (time-named (solve quarters))
              '((dollars . 14) (quarters . 12)))
(print-debug-info)


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

(check-equal? (length (time-named (solve* xsum))) 8)
(print-debug-info)


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

(define (word-value . xs)
  (for/sum ([(x idx) (in-indexed (reverse xs))])
           (* x (expt 10 idx))))

(define smm (make-csp))
(add-vars! smm '(s e n d m o r y) (λ () (range 10)))
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
(check-equal? (parameterize ([current-select-variable mrv-degree-hybrid]) ; todo: why is plain mrv so bad on this problem?
                (time-named (solve smm))) '((s . 9) (e . 5) (n . 6) (d . 7) (m . 1) (o . 0) (r . 8) (y . 2)))
(print-debug-info)

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

(check-equal? 92 (length (time-named (solve* queens))))
(print-debug-info)

#|
# There are no tricks, just pure logic, so good luck and don't give up. 
# 
# 1. In a street there are five houses, painted five different colours. 
# 2. In each house lives a person of different nationality 
# 3. These five homeowners each drink a different kind of beverage, smoke 
# different brand of cigar and keep a different pet. 
# 
# THE QUESTION: WHO OWNS THE zebra? 
# 
# HINTS 
# 
# 1. The englishman lives in a red house. 
# 2. The spaniard keeps dogs as pets. 
# 5. The owner of the Green house drinks coffee. 
# 3. The ukrainian drinks tea. 
# 4. The Green house is on the left of the ivory house. 
# 6. The person who smokes oldgold rears snails. 
# 7. The owner of the Yellow house smokes kools. 
# 8. The man living in the centre house drinks milk. 
# 9. The Norwegian lives in the first house. 
# 10. The man who smokes chesterfields lives next to the one who keeps foxes. 
# 11. The man who keeps horses lives next to the man who smokes kools. 
# 12. The man who smokes luckystrike drinks orangejuice. 
# 13. The japanese smokes parliaments. 
# 14. The Norwegian lives next to the blue house. 
# 15. The man who smokes chesterfields has a neighbour who drinks water. 
|#

(define (sym . args) (string->symbol (apply format args)))

(define zebra (make-csp))

(define ns (map (curry sym "nationality-~a") (range 5)))
(define cs (map (curry sym "color-~a") (range 5)))
(define ds (map (curry sym "drink-~a") (range 5)))
(define ss (map (curry sym "smoke-~a") (range 5)))
(define ps (map (curry sym "pet-~a") (range 5)))

(add-vars! zebra ns '(englishman spaniard ukrainian norwegian japanese))
(add-vars! zebra cs '(red ivory green yellow blue))
(add-vars! zebra ds '(tea coffee milk orange-juice water))
(add-vars! zebra ss '(oldgold kools chesterfields luckystrike parliaments))
(add-vars! zebra ps '(dogs snails foxes horses zebra))

(for ([vars (list ns cs ds ss ps)])
     (add-pairwise-constraint! zebra neq? vars))

(define (xnor lcond rcond)
  (or (and lcond rcond) (and (not lcond) (not rcond))))
(define (paired-with lval left rval right)
  (add-constraint! zebra (λ (left right) (xnor (eq? left lval) (eq? rval right))) (list left right)))

(define (paired-with* lval lefts rval rights)
  (for ([left lefts][right rights])
       (paired-with lval left rval right)))

;# 1. The englishman lives in a red house. 
('englishman ns . paired-with* . 'red cs)

;# 2. The spaniard keeps dogs as pets. 
('spaniard ns . paired-with* . 'dogs ps)

;# 5. The owner of the Green house drinks coffee. 
('green cs . paired-with* . 'coffee ds)

;# 3. The ukrainian drinks tea.
('ukrainian ns . paired-with* . 'tea ds)

;# 4. The Green house is on the left of the ivory house.
('green (drop-right cs 1) . paired-with* . 'ivory (drop cs 1))
(add-constraint! zebra (curry neq? 'ivory) (list 'color-0))
(add-constraint! zebra (curry neq? 'green) (list 'color-4))

;# 6. The person who smokes oldgold rears snails.
('oldgold ss . paired-with* . 'snails ps)

;# 7. The owner of the Yellow house smokes kools.
('yellow cs . paired-with* . 'kools ss)

;# 8. The man living in the centre house drinks milk.
(add-constraint! zebra (λ (d) (eq? d 'milk)) (list 'drink-2))

;# 9. The Norwegian lives in the first house.
(add-constraint! zebra (λ (x) (eq? x 'norwegian)) (list 'nationality-0))

(define (next-to lval lefts rval rights)
  (for ([righta (drop-right rights 2)]
        [left (cdr lefts)]
        [rightb (drop rights 2)])
       (add-constraint! zebra (λ (left righta rightb)
                                (or (not (eq? left lval)) (eq? righta rval) (eq? rval rightb)))
                        (list left righta rightb)))
  (for ([left (list (first lefts) (last lefts))]
        [right (list (second rights) (fourth rights))])
       (add-constraint! zebra (λ (left right) (xnor (eq? left lval) (eq? rval right)))
                        (list left right))))

;# 10. The man who smokes chesterfields lives next to the one who keeps foxes.
('chesterfields ss . next-to . 'foxes ps)

;# 11. The man who keeps horses lives next to the man who smokes kools.
('horses ps . next-to . 'kools ss)

;# 12. The man who smokes luckystrike drinks orangejuice.
('luckystrike ss . paired-with* . 'orange-juice ds)

;# 13. The japanese smokes parliaments.
('japanese ns . paired-with* . 'parliaments ss)

;# 14. The Norwegian lives next to the blue house.
('norwegian ns . next-to . 'blue cs)

;# 15. The man who smokes chesterfields has a neighbour who drinks water. 
('chesterfields ss . next-to . 'water ds)

(define (finish x)
  (apply map list (slice-at x 5)))

(check-equal? (parameterize ([current-select-variable mrv]
                             [current-random #f])
                (finish (time-named (solve zebra))))
              '(((nationality-0 . norwegian) (color-0 . yellow) (drink-0 . water) (smoke-0 . kools) (pet-0 . foxes))
                ((nationality-1 . ukrainian) (color-1 . blue) (drink-1 . tea) (smoke-1 . chesterfields) (pet-1 . horses))
                ((nationality-2 . englishman) (color-2 . red) (drink-2 . milk) (smoke-2 . oldgold) (pet-2 . snails))
                ((nationality-3 . japanese) (color-3 . green) (drink-3 . coffee) (smoke-3 . parliaments) (pet-3 . zebra))
                ((nationality-4 . spaniard) (color-4 . ivory) (drink-4 . orange-juice) (smoke-4 . luckystrike) (pet-4 . dogs))))
(print-debug-info)

(module+ main
  (begin
   (define-syntax n (λ (stx) #'10))
   (time-avg n (void (solve quarters)))
   (time-avg n (void (solve* xsum)))
   (time-avg n (void (solve smm)))
   (time-avg n (void (solve* queens)))
   (time-avg n (void (solve zebra)))))
  