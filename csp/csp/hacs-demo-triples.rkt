#lang br
(require csp sugar)

(define triples (make-csp))

(add-var! triples 'a (range 10 50))
(add-var! triples 'b (range 10 50))
(add-var! triples 'c (range 10 50))

(define (valid-triple? x y z)
  (= (expt z 2) (+ (expt x 2) (expt y 2))))
(add-constraint! triples valid-triple? '(a b c))

(require math/number-theory)
(add-constraint! triples coprime? '(a b c))

(add-constraint! triples <= '(a b))

(time-avg 10 (solve* triples))

(define (f)
  (for*/list ([a (in-range 10 50)]
              [b (in-range 10 50)]
              #:when (<= a b)
              [c (in-range 10 50)]
              #:when (and (coprime? a b c) (valid-triple? a b c)))
             `((a . ,a) (b . ,b) (c . ,c))))

(time-avg 10 (f))