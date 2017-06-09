#lang racket/base
(require racket/port)
(provide (all-defined-out) (all-from-out racket/port))

(define (port-position ip)
  (define-values (line col pos) (port-next-location ip))
  pos)