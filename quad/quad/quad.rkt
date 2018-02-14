#lang debug racket/base
(require racket/match racket/function "generic.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(struct $quad (attrs elems) #:transparent
  #:methods gen:quad
  [(define (elems q) ($quad-elems q))
   (define (attrs q) ($quad-attrs q))
   (define (entrance-point q) (hash-ref (attrs q) 'entrance 'entrance))
   (define (exit-point q) (hash-ref (attrs q) 'exit 'exit))
   (define (inner-point q) (hash-ref (attrs q) 'inner 'inner))
   (define (size q) (length (elems q)))
   (define (draw q) "<···>")])

(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elem? x) (or (char? x) (string? x) ($quad? x)))
(define (quad-elems? xs) (and (pair? xs) (andmap quad-elem? xs)))
(define (quad #:type [type $quad] . xs)
  (match xs
    [(list #f xs ...) (apply quad #:type type (hasheq) xs)]
    [(list (? quad-attrs? attrs) (? quad-elem? elems) ...) (type attrs elems)]
    [(list (? quad-elem? elems) ...) (apply quad #:type type #f elems)]
    [else (error 'bad-quad-input)]))
(define q quad)
(define (quads? xs) (andmap quad? xs))
(define (atomic-quad? x) (and (quad? x) (match (elems x)
                                          [(list (? char?)) #t]
                                          [else #f])))
(define (atomic-quads? xs) (andmap atomic-quad? xs))
(module+ test
  (check-true (atomic-quad? ($quad '#hasheq() '(#\H))))
  (check-true (atomic-quads? (list ($quad '#hasheq() '(#\H))))))


(struct $break $quad () #:transparent)
(define (break . xs) (apply quad #:type $break xs))
(define b break)

