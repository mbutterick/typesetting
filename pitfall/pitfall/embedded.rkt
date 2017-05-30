#lang pitfall/racket
(require "font.rkt")
(provide EmbeddedFont)

(define-subclass PDFFont (EmbeddedFont document font id)
  (super-new)
  (field #;[subset (· this font createSubset)]
         [unicode '((0))]
         #;[widths (list (send (send (· this font) getGlyph 0) advanceWidth))]
         
         [name (· font postscriptName)]
         [scale (/ 1000 (· font unitsPerEm))]
         [ascender (* (· font ascent) scale)]
         [descender (* (· font descent) scale)]
         [lineGap (* (· font lineGap) scale)]
         [bbox (· font bbox)])

  (as-methods
   widthOfString
   encode))

(define/contract (widthOfString this str size [features #f])
  ((string? number?) ((or/c list? #f)) . ->*m . number?)
  #|
PDFKit makes a whole layout here and measures that.
For now, we'll just measure width of the characters.
|#
  #;(define run (send (· this font) layout string)) ; todo: features would be passed here
  #;(define width (· run advanceWidth))
  #;(define scale (/ size (· this font unitsPerEm)))
  #;(* width scale)
  (send (· this font) measure-string str size))

;; called from text.rkt
(define/contract (encode this text [features #f])
  ((string?) ((or/c list? #f)) . ->*m . list?)
  (error 'embedded:encode-not-implemented))

(module+ test
  (require rackunit "fontkit.rkt")
  (define f (openSync "test/assets/Charter.ttf" #f))
  (define ef (make-object EmbeddedFont #f f #f))
  (check-equal? (send ef widthOfString "f" 1000) 321.0)
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef lineGap) 0)
  (check-equal? (· ef bbox) '(-161 -236 1193 963)))