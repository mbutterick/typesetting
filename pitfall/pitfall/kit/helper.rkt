#lang racket/base
(require (for-syntax racket/base) racket/class sugar/list racket/list)
(provide (all-defined-out))

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

(define-syntax-rule (push-end id thing) (set! id (append id (list thing))))

(module+ test
  (define xs '(1 2 3))
  (push-end xs 4)
  (check-equal? xs '(1 2 3 4))) 