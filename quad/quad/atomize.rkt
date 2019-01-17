#lang debug racket/base
(require racket/string
         racket/hash
         racket/match
         racket/list
         txexpr
         racket/function
         "quad.rkt"
         "param.rkt")
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (define h (make-hasheq))
  (apply hash-union! #:combine (λ (v1 v2) v2) h base-hash update-hashes)
  h)

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   (make-hasheq '((zim . "BANG") (foo . "zay") (toe . "jam")))))

(define whitespace-pat #px"\\s+")

(define (merge-and-isolate-white str)
  (for/list ([(m idx) (in-indexed (regexp-match* whitespace-pat str #:gap-select? #t))]
             #:when (non-empty-string? m))
            (if (even? idx) m " ")))

(define (merge-adjacent-strings xs [isolate-white? #false])
  (let loop ([xs xs][acc null])
    (match xs
      [(== empty) (reverse acc)]
      [(list (? string? strs) ..1 others ...)
       (loop others (append (reverse ((if isolate-white?
                                          merge-and-isolate-white
                                          list) (apply string-append strs))) acc))]
      [(cons x others) (loop others (cons x acc))])))

(define run-key 'run)

(define (same-run? qa qb)
  (eq? (hash-ref (quad-attrs qa) run-key) (hash-ref (quad-attrs qb) run-key)))

(define (atomize qx)
  ;; atomize a quad by reducing it to the smallest indivisible formatting units.
  ;; which are multi-character quads with the same formatting.
  (let loop ([x (make-quad qx)]
             [attrs (hash-copy (current-default-attrs))]
             [key (eq-hash-code (current-default-attrs))])
    (match-define-values (next-key next-attrs)
      ;; make a new run when we encounter non-empty attrs
      (match (quad-attrs x)
        [(? hash-empty?) (values key attrs)]
        [this-attrs (define next-key (eq-hash-code this-attrs))
                    (define next-attrs (attrs . update-with . this-attrs))
                    (hash-set! next-attrs run-key next-key)
                    (values next-key next-attrs)]))
    (match (quad-elems x)
      [(? pair? elems)
       (append* 
        (for/list ([elem (in-list (merge-adjacent-strings elems 'isolate-white))])
                  (match elem
                    [(? string?)
                     #|
190116
The conundrum: how to atomize quads that have subtypes and possibly other fields.
We need to make new quads derived from the original.
But we don't have access to the subtype here.
Making the quad mutable doesn't solve the problem: we can change the first one, but we still need copies.
`struct-copy` doesn't work, because it can't see the subtype.
`struct-list` doesn't work, because it can't rely on structs being transparent.
|#
                     (list (make-quad #:type (quad-type x)
                                      #:attrs next-attrs
                                      #:elems (list elem)))]
                    [_ (loop elem next-attrs next-key)])))]
      [_ (list x)])))

(module+ test
  (define (filter-private-keys qs)
    (for-each (λ (q) (when (hash-has-key? (quad-attrs q) 'run)
                       (hash-remove! (quad-attrs q) 'run))) qs)
    qs)
  (struct $br quad ())
  (define br (q #:type $br (hasheq 'br "time")))
  (check-equal? (filter-private-keys (atomize (q (q "a b") br (q "x y"))))
                (list (q "a") (q " ") (q "b") br (q "x") (q " ") (q "y")))  
  (check-equal?
   (filter-private-keys (atomize (q (hasheq 'foo 42) (q "Hi" "    idiot" (q (hasheq 'bar 84) "There") "Eve" "ry" "one"))))
   (list (q (hasheq 'foo 42) "Hi")
         (q (hasheq 'foo 42) " ")
         (q (hasheq 'foo 42) "idiot")
         (q (hasheq 'foo 42 'bar 84) "There")
         (q (hasheq 'foo 42) "Everyone"))))
