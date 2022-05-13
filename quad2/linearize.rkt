#lang debug racket/base
(require racket/match
         racket/hash
         racket/list
         racket/string
         "pipeline.rkt"
         "quad.rkt")
(provide (all-defined-out))

(define-pass (split-into-single-char-quads qs)
  ;; break list of quads into single characters (keystrokes)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (append*
   (for/list ([q (in-list qs)])
             (match q
               [(quad _ _ (list (? string? str)) _)
                (for/list ([c (in-string str)])
                          (struct-copy quad q [elems (list (string c))]))]
               [_ (list q)]))))

(define-pass (linearize qs)
  ;; convert a single quad into a list of quads, with the attributes propagated downward
  ;; every resulting quad should have at most one element
  #:pre (list-of quad?)
  #:post (list-of simple-quad?)
  (append*
   (for/list ([q (in-list qs)])
             (let loop ([q q][attrs-context (make-quad-attrs)]) ;; returns (list-of quad?)
               (define current-attrs (quad-attrs-union attrs-context (quad-attrs q)))
               (define (mq es) (make-quad #:tag (quad-tag q) #:attrs current-attrs #:elems es))
               (match (quad-elems q)
                 [(? null?) (list (mq null))]
                 [(? pair? elems)
                  (apply append (for/list ([e (in-list elems)])
                                          (cond
                                            [(quad? e) (loop e current-attrs)]
                                            [else (list (mq (list e)))])))])))))

(module+ test
  (define q (make-quad #:attrs (hasheq 'foo 42) #:elems (list (make-quad #:elems (list "Hi" "    idiot" (make-quad #:attrs (hasheq 'bar 84) #:elems '("There")) " Eve" "ry" "one" (make-quad #:attrs (hasheq 'zam 108) #:elems null))))))
  (define lqs (linearize (list q)))
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
           (struct-copy quad e0 [elems (list (string-join (append-map quad-elems qs-to-merge) ""))])])
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
                      ;; the "gaps" (parts that don't match pattern) are guaranteed to be at even indexes
                      ;; If string starts with a "gap", a zero-length separator is appended to the start.
                      ;; so we just ignore those.
                      (for/list ([(substr idx) (in-indexed (regexp-match* whitespace-pat str #:gap-select? #t))]
                                 #:unless (zero? (string-length substr)))
                                (struct-copy quad q [elems (list (if (even? idx) substr word-space))]))]
                     [_ (list q)]))))
                                       
(module+ test
  (define smlqs (split-whitespace mlqs)))

(define-pass (mark-text-runs qs)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (for ([q (in-list qs)]
        #:when (match (quad-elems q)
                 [(list (? string?) ..1) #t]
                 [_ #false]))
       (set-quad-tag! q 'text-run))
  qs)

(module+ test
  (mark-text-runs smlqs))

(define-pass (append-bop-and-eop qs)
  ;; force document to have one page
  #:pre (list-of simple-quad?)
  #:post (λ (qs) (match qs
                   [(list (? bop-quad?) (? simple-quad?) ... (? eop-quad?)) #true]
                   [_ #false]))
  (define bop (bop-quad #f (quad-attrs (first qs)) null #f))
  (define eop (eop-quad #f (quad-attrs (last qs)) null #f))
  (append (list bop) qs (list eop)))

(define-pass (append-boq-and-eoq qs)
  ;; attach the boq and eoq signals
  #:pre (list-of simple-quad?)
  #:post (λ (qs) (match qs
                   [(list (== boq) (? simple-quad?) ... (== eoq)) #true]
                   [_ #false]))
  (set-quad-attrs! boq (quad-attrs (first qs)))
  (set-quad-attrs! eoq (quad-attrs (last qs)))
  (append (list boq) qs (list eoq)))