#lang pitfall/racket
(require "font.rkt" "glyph-position.rkt" "glyphrun.rkt" "subset.rkt")
(provide EmbeddedFont)

(define-subclass PDFFont (EmbeddedFont document font id)
  (super-new)
  (field [subset (· this font createSubset)]
         [unicode (mhash 0 0)] ; always include the missing glyph (gid = 0)
         [widths (mhash 0 (send (send (· this font) getGlyph 0) advanceWidth))]
         ;; always include the width of the missing glyph (gid = 0)
         
         [name (· font postscriptName)]
         [scale (/ 1000 (· font unitsPerEm))]
         [ascender (* (· font ascent) scale)]
         [descender (* (· font descent) scale)]
         [lineGap (* (· font lineGap) scale)]
         [bbox (· font bbox)])

  (as-methods
   widthOfString
   encode
   embed))

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
  ((string?) ((or/c list? #f)) . ->*m .
             (list/c (listof string?) (listof (is-a?/c GlyphPosition))))
  (define glyphRun (send (· this font) layout text features))
  (define glyphs (· glyphRun glyphs))
  (define positions (· glyphRun positions))
  (define-values (subset-idxs new-positions)
    (for/lists (idxs posns)
      ([(glyph i) (in-indexed glyphs)]
       [posn (in-list positions)])
      (define gid (send (· this subset) includeGlyph (· glyph id)))
      (define subset-idx (~r gid #:base 16 #:min-width 4 #:pad-string "0"))
      (set-field! advanceWidth posn (· glyph advanceWidth))

      (hash-ref! (· this widths) gid (λ () (· posn advanceWidth)))
      (hash-ref! (· this unicode) gid (λ () (· glyph codePoints)))

      (send posn scale (· this scale))
      (values subset-idx posn)))
  (list subset-idxs new-positions))


(define/contract (embed this)
  (->m void?)
  (define isCFF (is-a? (· this subset) CFFSubset))
  (define fontFile (· this document ref))

  (when isCFF
    (hash-set! (· fontFile payload) 'Subtype "CIDFontType0C"))

  ;; todo
  ;; (send (send (· this subset) encodeStream) pipe fontFile)

  ;; todo
  ;; (define familyClass (send (· this font) has-table? #"OS/2"))
  (define familyClass 0)

  ;; todo: flags
  (define flags 0)

  ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
  (define tag (list->string (for/list ([i (in-range 6)])
                              (integer->char (random 65 (+ 65 26))))))
  (define name (string-append tag "+" (· this font postscriptName)))

  (define bbox (· this font bbox))
  (define descriptor (send (· this document) ref
                           (mhash
                            'Type "FontDescriptor"
                            'FontName name
                            'Flags flags
                            'FontBBox (map (curry * (· this scale))
                                          (list (· bbox minX) (· bbox minY)
                                           (· bbox maxX) (· bbox maxY)))
                            'ItalicAngle (· this font italicAngle)
                            'Ascent (· this ascender)
                            'Descent (· this descender)
                            'CapHeight (* (or (· this font capHeight) (· this sfont ascent)) (· this scale))
                            'XHeight (* (or (· this font xHeight) 0) (· this scale))
                            'StemV 0)))

  (· descriptor end)

  (define descendantFont (send (· this document) ref
                               (mhash
                                'Type "Font"
                                'Subtype (string-append "CIDFontType" (if isCFF "0" "2"))
                                'BaseFont name
                                'CIDSystemInfo
                                (mhash
                                 'Registry (String "Adobe")
                                 'Ordering (String "Identity")
                                 'Supplement 0)
                                'FontDescriptor descriptor
                                'W (cons 0 (for/list ([idx (in-range (length (hash-keys (· this widths))))])
                                           (hash-ref (· this widths) idx (λ () (error 'embed (format "hash key ~a not found" idx)))))))))

  (· descendantFont end)

  (hash-set*! (· this dictionary payload)
              'Type "Font"
              'Subtype "Type0"
              'BaseFont name
              'Encoding "Identity-H"
              'DescendantFonts (list descendantFont)
              'ToUnicode (· this toUnicodeCmap))

  (· this dictionary end)

  (error 'embed-unfinished)
  )


(module+ test
  (require rackunit "fontkit.rkt" "bbox.rkt")
  (define f (openSync "test/assets/Charter.ttf" #f))
  (define ef (make-object EmbeddedFont #f f #f))
  (check-equal? (send ef widthOfString "f" 1000) 321.0)
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef lineGap) 0)
  (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (· ef widths) (mhash 0 278))
  (check-equal? (send (send (· ef font) getGlyph H-gid) advanceWidth) 738)
  
  )