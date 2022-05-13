#lang debug racket/base
(require racket/contract
         racket/match
         racket/hash
         txexpr
         (for-syntax racket/base racket/syntax)
         "constants.rkt"
         "struct.rkt")
(provide (all-defined-out))

(struct $point (x y) #:transparent #:mutable)
(struct $size (width height) #:transparent #:mutable)
(struct $rect (origin size) #:transparent #:mutable)

(define current-wrap-width (make-parameter 5))
(define current-page-size (make-parameter ($size 10 10)))

(define (list-of proc) (λ (x) (and (list? x) (andmap proc x))))

(struct quad (tag attrs elems posn) #:transparent #:mutable
  #:constructor-name quad-constructor
  #:guard (λ (tag attrs elems posn name)
            (unless (match (list tag attrs elems)
                      [(list (? quad-tag?)
                             (? quad-attrs?)
                             (? quad-elems?)) #true]
                      [_ #false])
              (raise-user-error 'quad-constructor (format "failed: ~v" (list tag attrs elems posn))))
            (values tag attrs elems posn)))
             
(define (quad-tag? x) (match x
                        [(or (? symbol?) #false) #true]
                        [_ #false]))
(define (make-quad-attrs [alist null]) (make-hasheq alist))

(define (quad-attrs-union . attrss)
  (define qas (make-quad-attrs))
  (apply hash-union! #:combine (λ (v1 v2) v2) qas attrss)
  qas)

(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elems? x) (list? x))

(define/contract (make-quad #:tag [tag #false]
                            #:attrs [attrs (make-quad-attrs null)]
                            #:elems [elems null])
  (() (#:tag quad-tag? #:attrs (or/c quad-attrs? (listof any/c)) #:elems quad-elems?) . ->* . quad?)
  (let ([attrs (let loop ([attrs attrs])
                 (cond
                   [(list? attrs) (loop (apply hasheq attrs))]
                   [(immutable? attrs) (make-hasheq (hash->list attrs))]
                   [else attrs]))])
    (quad-constructor tag attrs elems #false)))

(define (quad-ref q-or-qs key
                  [default-val (λ () (error (format "quad-ref: no value for key ~a" key)))]
                  #:set-default-if-missing [set-default-if-missing? #false])
  (unless (attr-key? key)
    (raise-argument-error 'quad-ref "attr-key?" key))
  (define hash-reffer (if set-default-if-missing? hash-ref! hash-ref))
  (hash-reffer (quad-attrs (match q-or-qs
                             [(? quad? q) q]
                             [(cons q _) q]
                             [_ (raise-argument-error 'quad-ref "quad or list of quads" q-or-qs)])) key default-val))

(define (quad-set! q key val)
  (hash-set! (quad-attrs q) key val))

(define (quad-update! q key updater)
  (hash-update! (quad-attrs q) key updater))

(define (quad-ref! q-or-qs key default-val)
  (quad-ref q-or-qs key default-val #:set-default-if-missing #true))

(define (quad-has-key? q-or-qs key)
  (not (eq? (quad-ref q-or-qs key no-value-signal) no-value-signal)))

(define-syntax (define-quad-field stx)
  (syntax-case stx ()
    [(_ FIELD)
     (with-syntax ([GETTER (format-id stx "quad-~a" #'FIELD)]
                   [SETTER (format-id stx "set-quad-~a!" #'FIELD)])
       #'(begin        
           (define (GETTER q) (quad-ref q 'FIELD))
           (define (SETTER q val) (quad-set! q 'FIELD val))))]))

#;(define-quad-field posn)

(define (simple-quad? x)
  (and (quad? x) (<= (length (quad-elems x)) 1)))

(define (has-no-position? q) (not (has-position? q)))
(define (has-position? q) (quad-posn q))

(define (txexpr->quad x)
  (match x
    [(txexpr tag attrs elems)
     (make-quad #:tag tag
                #:attrs (attrs->hash attrs)
                #:elems (map txexpr->quad elems))]
    [_ x]))

(define (bootstrap-input x)
  ;; turn a simple string into a quad for testing layout.
  (let loop ([x x])
    (match x
      [(? quad? q) (list q)]
      [(and (list (? quad?) ...) qs) (loop (make-quad #:elems qs))]
      [(? txexpr? tx) (loop (txexpr->quad tx))]
      [(? string? str) (loop (make-quad #:elems (list str)))])))

(module+ test
  (define q (make-quad #:tag 'div #:attrs (make-hasheq '((hello . "world"))) #:elems (list "fine"))))

(define boq (let ()
              (struct boq-quad quad ())
              (boq-quad #f (make-hasheq) null #f)))
(define eoq (let ()
              (struct eoq-quad quad ())
              (eoq-quad #f (make-hasheq) null #f)))
(struct bop-quad quad ())
(struct eop-quad quad ())
