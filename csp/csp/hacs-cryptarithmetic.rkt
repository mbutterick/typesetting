#lang debug racket
(require "hacs.rkt" sugar/debug)
(module+ test (require rackunit))

(define (word-value d str)
    (define xs (for/list ([c (in-string str)])
                 (dict-ref d (string->symbol (string c)))))
    (for/sum ([(x idx) (in-indexed (reverse xs))])
      (* x (expt 10 idx))))

(define (math-csp str)
  (define input str)
  (define words (map string-downcase (string-split input)))
  (match-define (list terms ... sum) words)
  (define vars (map string->symbol (remove-duplicates (for*/list ([word words]
                                                                  [c word])
                                                        (string c)))))
  (unless (<= (length vars) 10)
    (raise-argument-error 'too-many-letters))

  (define (not= x y) (not (= x y)))

  (define math (make-csp))
  (add-vars! math vars (range 0 10))
  ;; all letters have different values
  (add-pairwise-constraint! math not= vars)
;; first letters cannot be zero
  (define firsts (remove-duplicates (map (compose1 string->symbol string car string->list) words) eq?))
  (for ([first firsts])
    (add-constraint! math positive? (list first)))
  (add-constraint! math (λ args
                          (define dict (map cons vars args))
                          (= (apply + (map (λ (w) (word-value dict w)) terms)) (word-value dict sum))) vars)
  math)

#;(solve (math-csp "TWO TWO FOUR"))
#;(solve (math-csp "DUCK DUCK GOOSE"))
#;(solve (math-csp "TICK TICK BOOM"))
#;(solve (math-csp "SEND MORE MONEY"))
#;(solve (math-csp "THIS THAT OTHER"))