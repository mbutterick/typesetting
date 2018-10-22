#lang racket

(require "problem.rkt" "constraint.rkt" sugar/debug)

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

(define ep (new problem%))

(for ([idx '(1 2 3 4 5)])
  (send ep add-variable (format "color~a" idx) '("red" "ivory" "green" "yellow" "blue"))
  
  (send ep add-variable (format "nationality~a" idx) '("englishman" "spaniard" "ukrainian" "norwegian" "japanese"))
  
  (send ep add-variable (format "drink~a" idx) '("tea" "coffee" "milk" "orangejuice" "water"))
  
  (send ep add-variable (format "smoke~a" idx) '("oldgold" "kools" "chesterfields" "luckystrike" "parliaments"))
  
  (send ep add-variable (format "pet~a" idx) '("dogs" "snails" "foxes" "horses" "zebra")))

(for ([name '("color" "nationality" "drink" "smoke" "pet")])
  (send ep add-constraint (new all-different-constraint%)
        (map (λ(idx) (format "~a~a" name idx)) '(1 2 3 4 5))))


(for ([idx '(1 2 3 4 5)])
  (send ep add-constraint 
        (λ(n c) (or (not (equal? n "englishman")) (equal? c "red")))
        (list (format "nationality~a" idx) (format "color~a" idx)))
  
  
  (send ep add-constraint 
        (λ(n p) (or (not (equal? n "spaniard")) (equal? p "dogs")))
        (list (format "nationality~a" idx) (format "pet~a" idx)))
  
  (send ep add-constraint 
        (λ(n d) (or (not (equal? n "ukrainian")) (equal? d "tea")))
        (list (format "nationality~a" idx) (format "drink~a" idx)))
  
  (if (< idx 5)
      (send ep add-constraint 
            (λ(ca cb) (or (not (equal? ca "green")) (equal? cb "ivory")))
            (list (format "color~a" idx) (format "color~a" (add1 idx))))
      (send ep add-constraint 
            (λ(c) (not (equal? c "green")))
            (list (format "color~a" idx))))
  
  (send ep add-constraint 
        (λ(c d) (or (not (equal? c "green")) (equal? d "coffee")))
        (list (format "color~a" idx) (format "drink~a" idx)))
  
  (send ep add-constraint 
        (λ(s p) (or (not (equal? s "oldgold")) (equal? p "snails")))
        (list (format "smoke~a" idx) (format "pet~a" idx)))
  
  (send ep add-constraint 
        (λ(c s) (or (not (equal? c "yellow")) (equal? s "kools")))
        (list (format "color~a" idx) (format "smoke~a" idx)))
  
  (when (= idx 3)
    (send ep add-constraint 
          (λ(d) (equal? d "milk"))
          (list (format "drink~a" idx))))
  
  (when (= idx 1)
    (send ep add-constraint 
          (λ(n) (equal? n "norwegian"))
          (list (format "nationality~a" idx))))
  
  (if (< 1 idx 5)
      (send ep add-constraint 
            (λ(s pa pb) (or (not (equal? s "chesterfields")) (equal? pa "foxes") (equal? pb "foxes")))
            (list (format "smoke~a" idx) (format "pet~a" (add1 idx)) (format "pet~a" (sub1 idx))))
      (send ep add-constraint 
            (λ(s p) (or (not (equal? s "chesterfields")) (equal? p "foxes")))
            (list (format "smoke~a" idx) (format "pet~a" (if (= idx 1) 2 4)))))
  
  (if (< 1 idx 5)
      (send ep add-constraint 
            (λ(p sa sb) (or (not (equal? p "horses")) (equal? sa "kools") (equal? sb "kools")))
            (list (format "pet~a" idx) (format "smoke~a" (add1 idx)) (format "smoke~a" (sub1 idx))))
      (send ep add-constraint 
            (λ(p s) (or (not (equal? p "horses")) (equal? s "kools")))
            (list (format "pet~a" idx) (format "smoke~a" (if (= idx 1) 2 4)))))
  
  (send ep add-constraint 
        (λ(s d) (or (not (equal? s "luckystrike")) (equal? d "orangejuice")))
        (list (format "smoke~a" idx) (format "drink~a" idx)))
  
  (send ep add-constraint 
        (λ(n s) (or (not (equal? n "japanese")) (equal? s "parliaments")))
        (list (format "nationality~a" idx) (format "smoke~a" idx)))
  
  
  (if (< 1 idx 5)
      (send ep add-constraint 
            (λ(n ca cb) (or (not (equal? n "norwegian")) (equal? ca "blue") (equal? cb "blue")))
            (list (format "nationality~a" idx) (format "color~a" (add1 idx)) (format "color~a" (sub1 idx))))
      (send ep add-constraint 
            (λ(n c) (or (not (equal? n "norwegian")) (equal? c "blue")))
            (list (format "nationality~a" idx) (format "color~a" (if (= idx 1) 2 4)))))
  
  
  )


(module+ main
  (require rackunit)

(define s (time (send ep get-solution)))

(define result
  (for*/list ([idx '(1 2 3 4 5)]
       [name '("nationality" "color" "drink" "smoke" "pet")])
  (define key (format "~a~a" name idx))
  (format "~a ~a" key (hash-ref s key))))

(check-equal? result '("nationality1 norwegian"
  "color1 yellow"
  "drink1 water"
  "smoke1 kools"
  "pet1 foxes"
  "nationality2 ukrainian"
  "color2 blue"
  "drink2 tea"
  "smoke2 chesterfields"
  "pet2 horses"
  "nationality3 englishman"
  "color3 red"
  "drink3 milk"
  "smoke3 oldgold"
  "pet3 snails"
  "nationality4 japanese"
  "color4 green"
  "drink4 coffee"
  "smoke4 parliaments"
  "pet4 zebra"
  "nationality5 spaniard"
  "color5 ivory"
  "drink5 orangejuice"
  "smoke5 luckystrike"
  "pet5 dogs")))