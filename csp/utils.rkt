#lang racket/base
(require racket/list racket/bool)

(provide (all-defined-out))

(module+ test (require rackunit))



(define (count_if pred xs)
  ;; Count the number of elements of seq for which the predicate is true.
  (length (filter-not false? (map pred xs))))

(module+ test
  (check-equal? (count_if procedure? (list 42 null max min)) 2))

(define (find_if pred xs)
  ;; If there is an element of seq that satisfies predicate; return it.
  (or (findf pred xs) null))

(module+ test
  (check-equal? (find_if procedure? (list 3 min max)) min)
  (check-equal? (find_if procedure? (list 1 2 3)) null))


(define (every pred xs)
  ;;;True if every element of seq satisfies predicate.
  (andmap pred xs))

(module+ test
  (check-true (every procedure? (list min max)))
  (check-false (every procedure? (list min 3))))


(define (argmin_random_tie xs proc)
  ;; Return an element with lowest fn(seq[i]) score; break ties at random.
  ;; Thus, for all s,f: argmin_random_tie(s, f) in argmin_list(s, f)
  (define assocs (map (λ(x) (cons (proc x) x)) xs))
  (define min-value (apply min (map car assocs)))
  (define min-xs (map cdr (filter (λ(a) (= min-value (car a))) assocs)))
  (list-ref min-xs (random (length min-xs))))

;(argmin_random_tie (list (range 0 4) (range 5 9) (range 10 13) (range 20 23)) length)