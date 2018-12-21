#lang racket/base
(provide (all-defined-out))
(define test-mode (make-parameter #f))
(define compress-streams? (make-parameter #f))
(define current-doc-offset (make-parameter 'doc-offset-not-initialized))