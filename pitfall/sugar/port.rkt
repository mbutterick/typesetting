#lang racket/base
(require racket/port)
(provide (all-defined-out) (all-from-out racket/port))

(define (port-position ip)
  (file-position ip))

(define (set-port-position! ip where)
  (file-position ip where))

(module+ test
  (require rackunit)
  (define ip (open-input-bytes (bytes 1 2 3 4)))
  (port-count-lines! ip)
  (check-equal? (port-position ip) 0)
  (check-equal? (read-byte ip) 1)
  (check-equal? (port-position ip) 1)
  (check-equal? (read-byte ip) 2)
  (set-port-position! ip 4)
  (check-equal? (port-position ip) 4)
  (check-equal? (read-byte ip) eof)
  (set-port-position! ip 0)
  (check-equal? (port-position ip) 0)
  (check-equal? (read-byte ip) 1))