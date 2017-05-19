#lang racket/base
(provide (all-defined-out))
(define test-mode (make-parameter #f))
(define compress-streams? (make-parameter #f))