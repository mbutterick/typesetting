#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))
(require/typed racket/list [flatten ((Listof QuadAttrPair) . -> . (Listof QuadAttrPair))]
               [empty? ((Listof Any) . -> . Boolean)]
               )
(require sugar/debug)
(provide (all-defined-out))

;; struct implementation

(define-type QuadAttrKey Symbol)
(define-type QuadAttrValue Any)
(define-type QuadAttrs (HashTable QuadAttrKey QuadAttrValue))
(define-type QuadList (Listof Quad))
(struct Quad ([attrs : QuadAttrs] [list : QuadList]) #:transparent
  #:property prop:sequence (λ(q) (Quad-list q)))

(define Quad-attr-ref
  (case-lambda
    [([q : Quad] [key : QuadAttrKey]) 
     (hash-ref (Quad-attrs q) key)]
    [([q : Quad] [key : QuadAttrKey] [default : QuadAttrValue]) 
     (hash-ref (Quad-attrs q) key (λ() default))]))


(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))
(define-type QuadAttrPair (Pairof QuadAttrKey QuadAttrValue))


(provide gather-common-attrs)
(: gather-common-attrs ((Listof Quad) . -> . (U False (Listof QuadAttrPair))))
(define (gather-common-attrs qs)
  (: check-cap (QuadAttrPair . -> . Boolean))
  (define (check-cap cap)
    (equal? (Quad-attr-ref (car qs) (car cap) attr-missing) (cdr cap)))
  (let loop 
    ([qs qs]
     [common-attr-pairs : (Listof QuadAttrPair) (if (Quad-attrs (car qs))
                                                    
                                                    (for/list ([kv-pair (in-hash-pairs (Quad-attrs (car qs)))] 
                                                               #:unless (member (car kv-pair) cannot-be-common-attrs))  
                                                      kv-pair)
                                                    null)])
    (cond
      [(null? common-attr-pairs) #f]
      [(null? qs) common-attr-pairs]
      [else (loop (cdr qs) (filter check-cap common-attr-pairs))])))


(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ Id) 
     (with-syntax (
                   [id (format-id #'Id "~a" (string->symbol (string-downcase (symbol->string (syntax->datum #'Id)))))]
                   [Ids? (format-id #'Id "~as?" #'Id)]
                   [Quads->Id (format-id #'Id "Quads->~a" #'Id)])
       #'(begin
           (struct Id Quad ())
           (define-predicate Ids? (Listof Id))
           ;; quad converter
           (: Quads->Id ((Listof Quad) . -> . Id))
           (define (Quads->Id qs)
             (Id #hash() '()))
           
           (provide id)
           (: id ((Listof (U QuadAttrKey QuadAttrValue)) . -> . Id))
           (define (id [attrs '()])
             (apply hash attrs))
           ))]))


(define quad= equal?)

(: quad-has-attr? (Quad QuadAttrKey . -> . Boolean))
(define (quad-has-attr? q key)
  (hash-has-key? (Quad-attrs q) key))


(define-quad-type Hello)
(define-quad-type Gbye)
(define h (Hello #hash((foo . bar)) (list (Hello #hash() '()))))
(define h2 (Quads->Hello '()))
(define g (Gbye #hash((foo . bar)) '()))
(gather-common-attrs (list h g))

(define-quad-type Word)
(define-quad-type Line)
(define-quad-type Page)
(define-quad-type Spacer)
(define-quad-type Block)