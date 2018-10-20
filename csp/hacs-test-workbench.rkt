#lang debug racket
(require sugar "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv-degree-hybrid)
(current-order-values shuffle)
(current-shuffle #true)


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

(define (paired-with lval left rval right)
  (add-constraint! zebra (λ (left right) (or (not (eq? left lval)) (eq? rval right))) (list left right)))

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
  (lval (drop-right lefts 1) . paired-with* . rval (drop rights 1))
  (lval (drop lefts 1) . paired-with* . rval (drop-right rights 1)))

;# 10. The man who smokes chesterfields lives next to the one who keeps foxes.
('chesterfields ss . next-to . 'foxes ps)

;# 11. The man who keeps horses lives next to the man who smokes kools.
;('horses ps . next-to . 'kools ss)

;# 12. The man who smokes luckystrike drinks orangejuice.
('luckystrike ss . paired-with* . 'orange-juice ds)

;# 13. The japanese smokes parliaments.
('japanese ns . paired-with* . 'parliaments ss)

;# 14. The Norwegian lives next to the blue house.
;('norwegian ns . next-to . 'water ds)

;# 15. The man who smokes chesterfields has a neighbour who drinks water. 
;('chesterfields ss . next-to . 'water ds)

(define (finish x)
  (apply map list (slice-at x 5)))

(map finish (list (time (solve zebra))))