#lang debug racket/base
(require racket/contract racket/match (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(struct $point (x y) #:transparent #:mutable)
(struct $size (width height) #:transparent #:mutable)
(struct $rect (origin size) #:transparent #:mutable)

(define current-wrap-width (make-parameter 5))
(define current-page-size (make-parameter ($size 10 10)))

(define (list-of proc) (λ (x) (and (list? x) (andmap proc x))))

(struct quad (tag attrs elems) #:transparent #:mutable
  #:constructor-name quad-constructor
  #:guard (λ (tag attrs elems name)
            (unless (match (list tag attrs elems)
                      [(list (? quad-tag?)
                             (? quad-attrs?)
                             (? quad-elems?)) #true]
                      [_ #false])
              (error 'no-dice))
            (values tag attrs elems)))
             
(define (quad-tag? x) (match x
                        [(or (? symbol?) #false) #true]
                        [_ #false]))
(define (make-quad-attrs alist) (make-hasheq alist))
(define (quad-attrs? x) (hash-eq? x))
(define (quad-elems? x) (list? x))

(define/contract (make-quad #:tag [tag #false]
                            #:attrs [attrs (make-quad-attrs null)]
                            #:elems [elems null])
  (() (#:tag quad-tag? #:attrs quad-attrs? #:elems quad-elems?) . ->* . quad?)
  (quad-constructor tag attrs elems))

(define (quad-ref q key [default-val #false])
  (hash-ref (quad-attrs q) key default-val))
(define (quad-set! q key val)
  (hash-set! (quad-attrs q) key val))

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

(define (has-no-position? q) (not (has-position? q)))
(define (has-position? q) (quad-posn q))

(module+ test
  (define q (make-quad #:tag 'div #:attrs (make-hasheq '((hello . "world"))) #:elems (list "fine"))))