#lang pitfall/racket
(require "font/standard-fonts.rkt" "font/afm.rkt" "reference.rkt")
(provide PDFFont PDFFont-open)

(define PDFFont
  (class object%
    (super-new)
    (field [dictionary #f]
           [embedded #f])

    (as-methods
     ref
     finalize)
    ))

(define (PDFFont-open document src family id)
  (cond
    [(string? src)
     (when (isStandardFont src)
       (make-object StandardFont document src id))]))


(define/contract (ref this)
  (->m (is-a?/c PDFReference))
  (unless (· this dictionary)
    (set-field! dictionary this (send (· this document) ref)))
  (· this dictionary))

(define/contract (finalize this)
  (->m void?)
  (unless (or (· this embedded) (not (· this dictionary)))
    (· this embed)
    (set-field! embedded this #t)))


(define StandardFont
  (class PDFFont
    (super-new)
    (init-field document name id)
    (field [font (make-object AFMFont ((hash-ref standard-fonts name)))]
           [ascender (· font ascender)]
           [descender (· font descender)]
           [bbox (· font bbox)]
           [lineGap (· font lineGap)])
    (as-methods
     embed
     encode
     widthOfString)))

(define/contract (embed this)
  (->m void?)
  (set-field! data (· this dictionary)
              (mhash 'Type "Font"
                     'BaseFont (· this name)
                     'Subtype "Type1"
                     'Encoding "WinAnsiEncoding"))
  (· this dictionary end))


(define/contract (encode this text [options #f])
  ((string?) ((or/c hash? #f)) . ->*m . (list/c (listof string?) (listof hash?)))
  (define this-font (· this font))
  (define encoded (send this-font encodeText text))
  (define glyphs (send this-font glyphsForString text))
  (define advances (send this-font advancesForGlyphs glyphs))
  (define positions
    (for/list ([(glyph i) (in-indexed glyphs)]
               [advance (in-list advances)])
      (hasheq 'xAdvance advance
              'yAdvance 0
              'xOffset 0
              'yOffset 0
              'advanceWidth (send this-font widthOfGlyph glyph)))) 
  (list encoded positions))


(define/contract (widthOfString this str size [options #f])
  ((string? number?) ((or/c hash? #f)) . ->*m . number?)
  (let* ([this-font (· this font)]
         [glyphs (send this-font glyphsForString str)]
         [advances (send this-font advancesForGlyphs glyphs)]
         [width (apply + advances)]
         [scale (/ size 1000)])
    (* width scale)))


(module+ test
  (define stdfont (make-object StandardFont #f "Helvetica" #f)))