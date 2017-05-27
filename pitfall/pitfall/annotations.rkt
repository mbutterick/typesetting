#lang pitfall/racket
(provide annotation-mixin)

(define (annotation-mixin [% mixin-tester%])
  (class %
    (super-new)

    (as-methods
     annotate
     link
     _convertRect)))

(define/contract (annotate this x y w h options)
  (number? number? number? number? hash? . ->m . object?)
  (hash-set*! options
              'Type "Annot"
              'Rect (send this _convertRect x y w h)
              'Border '(0 0 0))
  (hash-ref! options 'C
             (λ ()
               (unless (equal? (· options Subtype) "Link")
                 (send this _normalizeColor (or (· options color) '(0 0 0))))))
  (hash-remove! options 'color)

  (when (string? (· options Dest))
    (hash-update! options 'Dest (λ (val) (String val))))

  (for ([(k v) (in-hash options)])
    (hash-set! options (string->symbol (string-titlecase (symbol->string k))) v))

  (define ref (· this ref options))
  (push-field! annotations this ref)
  (· ref end)
  this)

  

(define/contract (link this x y w h url [options (mhash)])
  ((number? number? number? number? string?) (hash?) . ->*m . object?)
  (hash-set! options 'Subtype "Link")
  (hash-set! options 'A (send this ref (mhash 'S "URI"
                                              'URI (String url))))
  (send (· options A) end)
  (send this annotate x y w h options))


(define/contract (_convertRect this x1 y1 w h)
  (number? number? number? number? . ->m . (list/c number? number? number? number?))
  ;; flip y1 and y2
  (let ([y2 y1]
        [y1 (+ y1 h)]
        [x2 (+ x1 w)])
    (match-define (list m0 m1 m2 m3 m4 m5) (· this _ctm))
    (list
     (+ (* x1 m0) (* y1 m2) m4)
     (+ (* x1 m1) (* y1 m3) m5)
     (+ (* x2 m0) (* y2 m2) m4)
     (+ (* x2 m1) (* y2 m3) m5))))