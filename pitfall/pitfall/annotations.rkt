#lang racket/base
(require
  "core.rkt"
  "reference.rkt"
  racket/class
  racket/match
  sugar/unstable/dict)

(provide annotation-mixin)

(define (annotation-mixin [% object%])
  (class %
    (super-new)
    (inherit-field @ctm)

    (define/public (annotate x y w h options)
      (hash-set*! options
                  'Type 'Annot
                  'Rect (convert-rect x y w h)
                  'Border '(0 0 0))
      (unless (eq? (hash-ref options 'Subtype #f) 'Link)
        (hash-ref! options 'C
                   (λ ()
                     (send this normalize-color (or (hash-ref options 'color #f) '(0 0 0))))))
      (hash-remove! options 'color)

      (for ([(k v) (in-hash options)])
        (hash-set! options (string->symbol (string-titlecase (symbol->string k))) v))

      (define annots-ref (make-ref options))
      (send (send this page) annotations annots-ref)
      (ref-end annots-ref)
      this)

    (define/public (link x y w h url [options (mhasheq)])
      (hash-set*! options
                  'Subtype 'Link
                  'A (make-ref (mhash 'S 'URI
                                      'URI url)))
      (ref-end (hash-ref options 'A))
      (annotate x y w h options))

    (define/public (convert-rect x1 y1 w h)
      ;; flip y1 and y2
      (let ([y2 y1]
            [y1 (+ y1 h)]
            [x2 (+ x1 w)])
        (match-define (list m0 m1 m2 m3 m4 m5) @ctm)
        (let* ([x1 (+ (* x1 m0) (* y1 m2) m4)]
               [y1 (+ (* x1 m1) (* y1 m3) m5)]
               [x2 (+ (* x2 m0) (* y2 m2) m4)]
               [y2 (+ (* x2 m1) (* y2 m3) m5)])
          (list x1 y1 x2 y2))))))

