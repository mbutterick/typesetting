#lang debug racket/base
(require racket/contract racket/match (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(struct $point (x y) #:transparent #:mutable)
(struct $size (width height) #:transparent #:mutable)
(struct $rect (origin size) #:transparent #:mutable)

(define current-wrap-width (make-parameter 5))
(define current-page-size (make-parameter ($size 10 10)))

(define (quad? x)
  (match x
    [($quad (? quad-tag?)
             (list (cons symbol? _) ...)
             (list _ ...)) #true]
    [_ #false]))

(struct $quad (tag attrs elems) #:transparent #:mutable)

(define quad-tag $quad-tag)
(define (quad-tag? x) (match x
                        [(or (? symbol?) #false) #true]
                        [_ #false]))
(define set-quad-tag! set-$quad-tag!)
(define quad-attrs $quad-attrs)
(define (quad-attrs? x) (match x
                        [(list (cons symbol? _) ...) #true]
                        [_ #false]))
(define set-quad-attrs! set-$quad-attrs!)
(define quad-elems $quad-elems)
(define (quad-elems? x) (list? x))
(define set-quad-elems! set-$quad-elems!)

(define/contract (make-quad tag attrs . elems)
  ((quad-tag? quad-attrs?) #:rest quad-elems? . ->* . quad?)
  ($quad tag attrs elems))

(define (quad-ref q key [default-val #false]) (match (assq key (quad-attrs q))
                                             [#false default-val]
                                             [(cons _ val) val]))
(define (quad-set! q key val)
  (set-quad-attrs! q (cons (cons key val) (quad-attrs q))))

(define-syntax (define-quad-field stx)
  (syntax-case stx ()
    [(_ FIELD)
     (with-syntax ([GETTER (format-id stx "quad-~a" #'FIELD)]
                   [SETTER (format-id stx "set-quad-~a!" #'FIELD)])
     #'(begin        
         (define (GETTER q) (quad-ref q 'FIELD))
         (define (SETTER q val) (quad-set! q 'FIELD val))))]))

(define-quad-field posn)
(define-quad-field char)

(define q (make-quad 'div '((hello . "world")) "fine"))