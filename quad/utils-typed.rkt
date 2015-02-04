#lang typed/racket/base
(require sugar/list)
(require/typed racket/list [flatten (All (A) (Rec as (U Any (Listof as))) -> (Listof Any))])
(require (for-syntax racket/syntax racket/base) racket/string (except-in racket/list flatten) sugar/debug racket/bool hyphenate racket/function math/flonum)
(require "quads-typed.rkt" "world-typed.rkt" "measure-typed.rkt")


;; predicate for use below
(: list-of-mergeable-attrs? (Any . -> . Boolean))
(define (list-of-mergeable-attrs? xs)
  (and (list? xs) (andmap (λ(x) (or (quad? x) (quad-attrs? x) (hashable-list? x))) xs)))

;; faster than (listof pair?)
(: pairs? (Any . -> . Boolean))
(define (pairs? x) (and (list? x) (andmap pair? x)))

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
(provide join-attrs)
(: join-attrs ((Listof (U Quad QuadAttrs HashableList)) . -> . (Listof QuadAttrPair)))
(define (join-attrs quads-or-attrs-or-lists)
  ((inst append-map QuadAttrPair QuadAttrs) (inst hash->list QuadAttrKey QuadAttrValue) (map (λ(x)
                                                                                               (cond
                                                                                                 [(quad? x) (quad-attrs x)]
                                                                                                 [(quad-attrs? x) (cast x QuadAttrs)]
                                                                                                 [(hashable-list? x) (quadattrs (cast x (Listof Any)))]
                                                                                                 [else ;; something that will have no effect on result 
                                                                                                  (cast (hash) QuadAttrs)])) quads-or-attrs-or-lists)))


;; flatten merges attributes, but applies special logic suitable to flattening
;; for instance, resolving x and y coordinates.
(provide flatten-attrs)
(: flatten-attrs ((U Quad QuadAttrs) * . -> . QuadAttrs))
(define (flatten-attrs . quads-or-attrs-or-falses)
  (define all-attrs (join-attrs quads-or-attrs-or-falses))
  (define-values (x-attrs y-attrs other-attrs-reversed)
    (for/fold ([xas : (Listof QuadAttrPair) null]
               [yas : (Listof QuadAttrPair) null]
               [oas : (Listof QuadAttrPair) null])
              ([attr (in-list all-attrs)])
      (cond
        [(equal? (car attr) world:x-position-key) (values (cons attr xas) yas oas)]
        [(equal? (car attr) world:y-position-key) (values xas (cons attr yas) oas)]
        [else (values xas yas (cons attr oas))])))
  (: make-cartesian-attr (QuadAttrKey (Listof QuadAttrPair) . -> . (Listof QuadAttrPair)))
  (define (make-cartesian-attr key attrs) 
    (if (empty? attrs) 
        empty 
        (list (cons key (apply + (cast ((inst map QuadAttrValue QuadAttrPair) cdr attrs) (Listof Flonum)))))))
  (define x-attr (make-cartesian-attr world:x-position-key x-attrs))
  (define y-attr (make-cartesian-attr world:y-position-key y-attrs))
  (for/hash : QuadAttrs ([kv-pair (in-list (append x-attr y-attr (reverse other-attrs-reversed)))])
    (values (car kv-pair) (cdr kv-pair))))

;; merge concatenates attributes, with later ones overriding earlier.
;; most of the work is done by join-attrs.
(provide merge-attrs)
(: merge-attrs ((U Quad QuadAttrs HashableList) * . -> . QuadAttrs))
(define (merge-attrs . quads-or-attrs-or-lists)
  (for/hash : QuadAttrs ([kv-pair (in-list (join-attrs quads-or-attrs-or-lists))])
    (values (car kv-pair) (cdr kv-pair))))



;; pushes attributes down from parent quads to children, 
;; resulting in a flat list of quads.
(provide flatten-quad)
(: flatten-quad (Quad . -> . (Listof Quad)))
(define (flatten-quad q)
  (cast (flatten 
         (let loop : (Treeof Quad)
           ([x : QuadListItem q][parent : Quad (box)])
           
           (cond       
             [(quad? x)
              (let ([x-with-parent-attrs (quad (quad-name x) 
                                               (flatten-attrs parent x) ; child positioned last so it overrides parent attributes 
                                               (quad-list x))])
                (if (empty? (quad-list x))
                    x-with-parent-attrs ; no subelements, so stop here
                    ((inst map (Treeof Quad) QuadListItem) (λ(xi) (loop xi x-with-parent-attrs)) (quad-list x))))] ; replace quad with its elements
             [else ;; it's a string
              (quad (quad-name parent) (quad-attrs parent) (list x))]))) (Listof Quad)))


;; flatten quad as above, 
;; then dissolve it into individual character quads while copying attributes
;; input is often large, so macro allows us to avoid allocation
(provide split-quad)
(: split-quad (Quad . -> . (Listof Quad)))
(define (split-quad q)
  (: do-explode ((QuadListItem) (Quad) . ->* . (Treeof Quad)))
  (define (do-explode x [parent (box)])
    (cond
      [(quad? x)
       (if (empty? (quad-list x))
           x ; no subelements, so stop here
           ((inst map (Treeof Quad) QuadListItem) (λ(xi) (do-explode xi x)) (quad-list x)))] ; replace quad with its elements, exploded
      [else ;; it's a string
       ((inst map (Treeof Quad) QuadListItem) (λ(xc) (quad 'word (quad-attrs parent) (list xc))) (regexp-match* #px"." x))]))
  (cast (flatten (map do-explode (flatten-quad q))) (Listof Quad)))

