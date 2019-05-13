#lang racket/base
(require racket/match racket/port racket/dict racket/struct)
(provide (all-defined-out))

;; structs
(define verbose-pitfall-printing? (make-parameter #f))

(struct pdf (width
             height
             pages
             refs
             root
             info
             opacity-registry
             current-fill-color
             ctm
             ctm-stack
             font-families
             current-font-features
             current-font-size
             current-font
             registered-fonts
             font-count
             line-gap
             x
             y
             image-registry
             output-path) #:transparent #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'pitfall-pdf)
      (λ (obj) (append
                (list (pdf-output-path obj))
                (if (verbose-pitfall-printing?)
                    (list 'other-pdf-fields)
                    null)))))])

(struct pdf-font (name
                  id
                  ascender
                  descender
                  upm
                  line-gap
                  bbox
                  ref
                  embedded
                  embed
                  encode
                  measure-string) #:transparent #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'pitfall-font)
      (λ (obj) (append
                (list (pdf-font-name obj))
                (if (verbose-pitfall-printing?)
                    (list 'other-pdf-font-fields)
                    null)))))])

;; for JPEG and PNG
(struct $img (data label width height ref embed-proc) #:transparent #:mutable)

;; for reference
(struct $ref (id payload offset port) #:transparent #:mutable
  #:methods gen:dict
  [(define (dict-ref ref key [thunk (λ () (error 'dict-ref-key-not-found))])
     (hash-ref ($ref-payload ref) key))
   (define (dict-ref! ref key thunk)
     (hash-ref! ($ref-payload ref) key thunk))
   (define (dict-set! ref key val) (hash-set! ($ref-payload ref) key val))
   (define (dict-update! ref key updater [failure-result (λ () (error 'update-no-key))])
     (hash-update! ($ref-payload ref) key updater failure-result))])

;; params


(define test-mode (make-parameter #f))
(define current-compress-streams (make-parameter #f))

(define current-pdf-version (make-parameter 1.3))
(define current-auto-first-page (make-parameter #t))
(define current-auto-helvetica (make-parameter #t))

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


