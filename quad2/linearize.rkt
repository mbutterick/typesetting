#lang debug racket/base
(require racket/match
         racket/hash
         racket/list
         racket/string
         "pipeline.rkt"
         "quad.rkt")
(provide (all-defined-out))

(define (simple-quad? x) (and (quad? x) (<= (length (quad-elems x)) 1)))

(define-pass (linearize q)
  ;; convert a single quad into a list of quads, with the attributes propagated downward
  ;; every resulting quad should have at most one element
  #:pre quad?
  #:post (list-of simple-quad?)
  (let loop ([q q][attrs-context (make-quad-attrs)]) ;; returns (list-of quad?)
    (define current-attrs (quad-attrs-union attrs-context (quad-attrs q)))
    (define (mq es) (make-quad #:tag (quad-tag q) #:attrs current-attrs #:elems es))
    (match (quad-elems q)
      [(? null?) (list (mq null))]
      [(? pair? elems)
       (apply append (for/list ([e (in-list elems)])
                               (cond
                                 [(quad? e) (loop e current-attrs)]
                                 [else (list (mq (list e)))])))])))

(module+ test
  (define q (make-quad #:attrs (hasheq 'foo 42) #:elems (list (make-quad #:elems (list "Hi" "    idiot" (make-quad #:attrs (hasheq 'bar 84) #:elems '("There")) " Eve" "ry" "one" (make-quad #:attrs (hasheq 'zam 108) #:elems null))))))
  (define lqs (linearize q))
  lqs)


(define-pass (merge-adjacent-strings sqs)
  ;; merge quads with the same attrs, and one or zero string elements,
  ;; into a single quad with one string element
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (let merge ([sqs sqs])
    (match sqs
      [_ #:when (<= (length sqs) 1) ; nothing to merge
         sqs]
      [(cons e0 rest)
       ;; because we copied attrs downward in linearize, we can use eq? to compare if they're the same
       ;; (instead of a key-by-key comparison)
       (define (attrs-same? e) (eq? (quad-attrs e0) (quad-attrs e)))
       (define-values (head tail) (splitf-at rest attrs-same?))
       (cons
        (cond
          [(null? head) e0]
          [else
           (define qs-to-merge (cons e0 head))
           (make-quad #:tag (quad-tag e0)
                      #:attrs (quad-attrs e0)
                      #:elems (list (string-join (append-map quad-elems qs-to-merge) "")))])
        (merge tail))])))

(module+ test
  (define mlqs (merge-adjacent-strings lqs))
  mlqs)


(define-pass (split-whitespace qs)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (define whitespace-pat #px"\\s+")
  (define word-space " ")
  (apply append
         (for/list ([q (in-list qs)])
                   (match (quad-elems q)
                     [(list (? string? str))
                      (define tag (quad-tag q))
                      (define attrs (quad-attrs q))
                      ;; the "gaps" (parts that don't match pattern) are guaranteed to be at even indexes
                      ;; If string starts with a "gap", a zero-length separator is appended to the start.
                      ;; so we just ignore those.
                      (for/list ([(substr idx) (in-indexed (regexp-match* whitespace-pat str #:gap-select? #t))]
                                 #:unless (zero? (string-length substr)))
                                (make-quad #:tag tag
                                           #:attrs attrs
                                           #:elems (list (if (even? idx) substr word-space))))]
                     [_ (list q)]))))
                                       
(module+ test
  (split-whitespace mlqs))
