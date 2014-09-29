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
  