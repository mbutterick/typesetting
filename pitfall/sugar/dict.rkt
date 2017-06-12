#lang racket/base
(require sugar/list)
(provide (all-defined-out))

(define (assoc? x) (and (pair? x) (not (list? x))))
(define (assocs? xs) (and (list? xs) (andmap assoc? xs)))

(define (listify kvs)
  (for/list ([slice (in-list (slice-at kvs 2))])
            (cons (car slice) (cadr slice))))
(define-syntax-rule (define-hashifier id hasher) (define (id . kvs) (hasher (listify kvs))))

;; like indefinite-arity `hash` but mutable
(define-hashifier mhash make-hash)
(define-hashifier mhasheq make-hasheq)
(define-hashifier mhasheqv make-hasheqv)

(module+ test
  (require rackunit)
  (check-equal? (mhash 'k "v") (make-hash (list (cons 'k "v"))))) 

(define (dictify . xs) (listify xs))