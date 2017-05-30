#lang pitfall/racket
(require "font.rkt")
(provide EmbeddedFont)


(define-subclass PDFFont (EmbeddedFont document font id)
  (super-new)
  (field [name (· font postscriptName)]
         [scale (/ 1000 (· font unitsPerEm))]
         [ascender (* (· font ascent) scale)]
         [descender (* (· font descent) scale)])

  (as-methods
   widthOfString
   encode))

(define/contract (widthOfString this str size [features #f])
  ((string? number?) ((or/c list? #f)) . ->*m . number?)
  #;(define run (send (· this font) layout string)) ; todo: features would be passed here
  #;(define width (· run advanceWidth))
  #;(define scale (/ size (· this font unitsPerEm)))
  #;(* width scale)
  (send (· this font) measure-string str size))

;; called from text.rkt
(define/contract (encode this text [features #f])
  ((string?) ((or/c list? #f)) . ->*m . list?)
  (report '(0)))