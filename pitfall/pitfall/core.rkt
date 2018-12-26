#lang racket/base
(require racket/match racket/port racket/dict)
(provide (all-defined-out))

;; structs

(struct $img (data label width height obj embed-proc) #:transparent #:mutable)

(struct $ref (id payload offset port) #:transparent #:mutable
  #:methods gen:dict
  [(define (dict-ref $ key [thunk (λ () (error 'dict-ref-key-not-found))])
     (hash-ref ($ref-payload $) key))
   (define (dict-ref! $ key thunk)
     (hash-ref! ($ref-payload $) key thunk))
   (define (dict-set! $ key val) (hash-set! ($ref-payload $) key val))
   (define (dict-update! $ key updater [failure-result (λ () (error 'update-no-key))])
     (hash-update! ($ref-payload $) key updater failure-result))])

;; for JPEG and PNG
(struct image (label width height obj) #:transparent #:mutable)

;; for page
(struct margin (top left bottom right) #:transparent #:mutable)

;; params

(define test-mode (make-parameter #f))
(define current-compress-streams? (make-parameter #f))

(define current-pdf-version (make-parameter 1.3))
(define current-auto-first-page (make-parameter #t))
(define current-auto-helvetica (make-parameter #t))

(define current-default-margins (make-parameter (margin 72 72 72 72)))

(define current-font (make-parameter #f))
(define current-font-size (make-parameter 12))

;; helpers

(define (numberizer x #:round [round? #true])
  (unless (and (number? x) (< -1e21 x 1e21))
    (raise-argument-error 'number "valid number" x))
  (let ([x (if round? (/ (round (* x 1e6)) 1e6) x)])
    (number->string (if (integer? x)
                        (inexact->exact x)
                        x))))

(define (to-bytes x)
  (match x
    [(? bytes?) x]
    [(? input-port?) (port->bytes x)]
    [_ (string->bytes/latin-1 (string-append x "\n"))]))

(define (write-bytes-out x)
  (void (write-bytes (to-bytes x))))

(define (bounded low x high)
  (if (high . < . low)
      (bounded high x low)
      (max low (min high x))))

(module+ test
  (require rackunit)
  (check-equal? (bounded 0 2 1) 1)
  (check-equal? (bounded 1 2 0) 1)
  (check-equal? (bounded 0 -2 1) 0)
  (check-equal? (bounded 1 -2 0) 0)
  (check-equal? (bounded 0 .5 1) 0.5)
  (check-equal? (bounded 0 0 1) 0)
  (check-equal? (bounded 0 1 1) 1))


