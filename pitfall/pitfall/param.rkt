#lang racket/base
(provide (all-defined-out))
(define test-mode (make-parameter #f))
(define compression-enabled (make-parameter #f))