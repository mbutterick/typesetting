#lang racket/base
(provide (all-defined-out))

;; structs

(struct String (string) #:transparent)

;; for JPEG and PNG
(struct image (label width height obj) #:transparent #:mutable)

;; params

(define test-mode (make-parameter #f))
(define current-compress-streams? (make-parameter #f))

(define current-pdf-version (make-parameter 1.3))
(define current-auto-first-page (make-parameter #t))
(define current-doc-offset (make-parameter 'doc-offset-not-initialized))

;; helpers

(define (numberizer x #:round [round? #true])
  (unless (and (number? x) (< -1e21 x 1e21))
    (raise-argument-error 'number "valid number" x))
  (let ([x (if round? (/ (round (* x 1e6)) 1e6) x)])
    (number->string (if (integer? x)
                        (inexact->exact x)
                        x))))