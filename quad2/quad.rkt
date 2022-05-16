#lang debug racket/base
(require racket/contract
         racket/match
         racket/hash
         racket/list
         racket/string
         racket/format
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

(define-syntax-rule (auto-struct NAME (FIELD ...) . ARGS)
  (struct NAME (FIELD ...) . ARGS))

(auto-struct quad (tag attrs elems origin size)
             #:transparent #:mutable
             #:constructor-name quad-new
             #:methods gen:custom-write
             [(define (write-proc val out mode)
                ;; cdr because struct->vector puts struct descriptor in first slot
                (define fields (cdr (vector->list (struct->vector val))))
                ;; cdr because tag is in first position
                (fprintf out (format "<~a>" (string-join (cons (~a (or (quad-tag val) "quad")) (map ~v (filter values (cdr fields)))) " "))))])

(define (quad-new-default)
  (apply quad-new (make-list (procedure-arity quad-new) #f)))

(define-syntax-rule (quad-copy Q . ARGS)
  ;; TODO: struct-copy is questionable
  (struct-copy quad Q . ARGS))
             
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
                            #:attrs [attrs #f]
                            #:elems [elems #f])
  (() (#:tag quad-tag? #:attrs (or/c quad-attrs? (listof any/c)) #:elems quad-elems?) . ->* . quad?)
  (let ([attrs (let loop ([attrs attrs])
                 (cond
                   [(list? attrs) (loop (apply hasheq attrs))]
                   [(immutable? attrs) (make-hasheq (hash->list attrs))]
                   [else attrs]))])
    (define newq (quad-new-default))
    (when tag (set-quad-tag! newq tag))
    (when attrs (set-quad-attrs! newq attrs))
    (when elems (set-quad-elems! newq elems))
    newq))

(define (initialize-attrs! q)
  (set-quad-attrs! q (make-hasheq)))

(define (quad-ref q-or-qs key
                  [default-val (λ () (error (format "quad-ref: no value for key ~a" key)))]
                  #:set-default-if-missing [set-default-if-missing? #false])
  (unless (attr-key? key)
    (raise-argument-error 'quad-ref "attr-key?" key))
  (define hash-reffer (if set-default-if-missing? hash-ref! hash-ref))
  (define q (match q-or-qs
              [(? quad? q) q]
              [(cons q _) q]
              [_ (raise-argument-error 'quad-ref "quad or list of quads" q-or-qs)]))
  (when (and set-default-if-missing? (not (quad-attrs q)))
    (set-quad-attrs! q (make-hasheq)))
  (match (quad-attrs q)
    [(? quad-attrs? attrs) (hash-reffer attrs key default-val)]
    [_ (default-val)]))

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
  (and (quad? x) (if (list? (quad-elems x))
                     (<= (length (quad-elems x)) 1)
                     #true)))

(define (has-no-position? q) (not (has-position? q)))
(define (has-position? q) (quad-origin q))

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

(define bod (make-quad #:tag 'bod-quad))
(define eod (make-quad #:tag 'eod-quad))
(define (bop-quad) (make-quad #:tag 'bop-quad))
(define (bop-quad? x) (and (quad? x) (eq? (quad-tag x) 'bop-quad)))
(define (eop-quad) (make-quad #:tag 'eop-quad))
(define (eop-quad? x) (and (quad? x) (eq? (quad-tag x) 'eop-quad)))

(define (insert-at-beginning qs x)
  (unless (andmap quad? qs)
    (raise-argument-error 'insert-at-beginning "list of quads" qs))
  (unless (quad? x)
    (raise-argument-error 'insert-at-beginning "quad" x))
  (cond
    [(pair? qs)
     (set-quad-attrs! x (quad-attrs (first qs)))
     (cons x qs)]
    [else (list x)]))

(define (insert-at-end qs x)
  (unless (andmap quad? qs)
    (raise-argument-error 'insert-at-end "list of quads" qs))
  (unless (quad? x)
    (raise-argument-error 'insert-at-end "quad" x))
  (cond
    [(pair? qs)
     (set-quad-attrs! x (quad-attrs (last qs)))
     (append qs (list x))]
    [else (list x)]))