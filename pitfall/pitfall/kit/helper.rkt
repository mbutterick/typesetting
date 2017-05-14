#lang racket/base
(require (for-syntax racket/base) racket/class sugar/list racket/list (only-in br/list push! pop!))
(provide (all-defined-out) push! pop!)

(define-syntax (· stx)
  (syntax-case stx ()
    [(_ x ref)
     #'(cond
         [(object? x) (with-handlers ([exn:fail:object? (λ (exn) (send x ref))])
                        (get-field ref x))]
         [(hash? x) (hash-ref x 'ref)]
         [else (raise-argument-error '· "object or hash" x)])]
    [(_ x ref0 . refs) #'(· (· x ref0) . refs)]))

(define-syntax (·map stx)
  (syntax-case stx ()
    [(_ ref xs) #'(for/list ([x (in-list xs)]) (· x ref))]))

(define-syntax-rule (+= id thing) (set! id (+ id thing))) 
(define-syntax-rule (++ id) (+= id 1))
(define-syntax-rule (-- id) (+= id -1))
(define-syntax-rule (-= id thing) (+= id (- thing)))

(module+ test
  (require rackunit)
  (define C
    (class object%
      (super-new)
      (field [foo 'field])
      (define/public (bar) 'method)
      (define/public (zam) (hasheq 'zoom 'hash))))
  (define h (hasheq 'bam (new C) 'foo 'hashlet))
  (define o (new C))
  (check-equal? (· o foo) 'field)
  (check-equal? (· o bar) 'method)
  (check-equal? (· o zam zoom) 'hash)
  (check-equal? (· h bam foo) 'field)
  (check-equal? (· h bam bar) 'method)
  (check-equal? (· h bam zam zoom) 'hash)
  (check-equal? (·map foo (list o h)) '(field hashlet)))


(define (listify kvs)
  (for/list ([slice (in-list (slice-at kvs 2))])
            (cons (first slice) (second slice))))
(define-syntax-rule (define-hashifier id hasher) (define (id . kvs) (hasher (listify kvs))))
(define-hashifier mhash make-hash)
(define-hashifier mhasheq make-hasheq)
(define-hashifier mhasheqv make-hasheqv)

(module+ test
  (check-equal? (mhash 'k "v") (make-hash (list (cons 'k "v"))))) 


(define isBuffer? bytes?)
(define (newBuffer x) (string->bytes/latin-1 (format "~a" x)))
(define buffer-length bytes-length)


;; js-style `push`, which appends to end of list
(define-syntax-rule (push-end! id thing) (set! id (append id (list thing))))

(module+ test
  (define xs '(1 2 3))
  (push-end! xs 4)
  (check-equal? xs '(1 2 3 4)))

;; fancy number->string. bounds are checked, inexact integers are coerced.
(define (number x)
  (unless (< -1e21 x 1e21)
    (raise-argument-error 'number "valid number" x))
  (let ([x (/ (round (* x 1e6)) 1e6)])
    (number->string (if (integer? x)
                        (inexact->exact x)
                        x))))

(module+ test
  (check-equal? (number 4.5) "4.5")
  (check-equal? (number 4.0) "4")
  (check-equal? (number 4) "4"))


(define-syntax (send*/fold stx)
  (syntax-case stx ()
    [(_ o) #'o]
    [(_ o [m0 . args0] [m . args] ...)
     #'(send*/fold (send o m0 . args0) [m . args] ...)]))

(module+ test
  (define SFC (class object%
                (super-new)
                (field [sum 0])
                (define/public (add x) (set! sum (+ sum x)) this)))
  (define sfo (new SFC))
  (check-equal? (get-field sum (send*/fold sfo [add 1] [add 2] [add 3])) 6))