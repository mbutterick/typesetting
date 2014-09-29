#lang racket/base
(require racket/list racket/bool)

(provide (all-defined-out))

(module+ test (require rackunit))

(define (count_if pred xs)
  (length (filter-not false? (map pred xs))))

(module+ test
  (check-equal? (count_if procedure? (list 42 null max min)) 2))