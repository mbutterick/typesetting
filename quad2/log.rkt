#lang racket/base

(provide (all-defined-out))

;; creates `quad2-logger` and associated functions:
;; log-quad2-fatal, log-quad2-error, log-quad2-warning, 
;; log-quad2-info, and log-quad2-debug
(define-logger quad2) 