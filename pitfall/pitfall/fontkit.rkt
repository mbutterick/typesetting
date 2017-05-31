#lang pitfall/racket
(require "freetype-ffi.rkt" ffi/unsafe racket/runtime-path "subset.rkt" "glyph.rkt" "layout-engine.rkt" "bbox.rkt" "glyphrun.rkt" "cmap-processor.rkt")
(provide (all-defined-out))

(define-runtime-path charter-path "test/assets/charter.ttf")

;; approximates
;; https://github.com/devongovett/fontkit/blob/master/src/TTFFont.js

(define-subclass object% (TTFFont filename)
  (super-new)
  (field [_tables (mhash)]
         [_glyphs (mhash)]
         [_layoutEngine #f])
  
  (field [buffer (file->bytes filename)])

  (define (buffer->font buffer)
    'made-ttf-font)

  (define (probe buffer)
    (and
     (member (bytes->string/latin-1 (subbytes buffer 0 4))
             (list "true" "OTTO" "\u0\u1\u0\u0"))
     'TTF-format))
  
  (and (probe buffer) (buffer->font buffer))

  (field [ft-library (FT_Init_FreeType)])
  (field [ft-face (FT_New_Face ft-library charter-path 0)])

  (as-methods
   postscriptName
   measure-string
   unitsPerEm
   ascent
   descent
   lineGap
   bbox
   createSubset
   has-table?
   has-cff-table?
   has-morx-table?
   has-gsub-table?
   has-gpos-table?
   getGlyph
   layout
   glyphsForString
   glyphForCodePoint))


;; The unique PostScript name for this font
(define/contract (postscriptName this)
  (->m string?)
  (FT_Get_Postscript_Name (· this ft-face)))


;; The size of the font’s internal coordinate grid
(define/contract (unitsPerEm this)
  (->m number?)
  (FT_FaceRec-units_per_EM (· this ft-face)))


;; The font’s [ascender](https://en.wikipedia.org/wiki/Ascender_(typography))
(define/contract (ascent this)
  (->m number?)
  (FT_FaceRec-ascender (· this ft-face)))


;; The font’s [descender](https://en.wikipedia.org/wiki/Descender)
(define/contract (descent this)
  (->m number?)
  (FT_FaceRec-descender (· this ft-face)))


;; The amount of space that should be included between lines
(define/contract (lineGap this)
  (->m number?)
  (define hhea-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_hhea) _pointer _FT_HoriHeader-pointer))
  (FT_HoriHeader-lineGap hhea-table))


;; The font’s bounding box, i.e. the box that encloses all glyphs in the font.
(define/contract (bbox this)
  (->m (is-a?/c BBox))
  (let ([bbox (FT_FaceRec-bbox (· this ft-face))])
    (make-object BBox (FT_BBox-xMin bbox)
      (FT_BBox-yMin bbox)
      (FT_BBox-xMax bbox)
      (FT_BBox-yMax bbox))))


(define/contract (_cmapProcessor this)
  (->m (is-a?/c CmapProcessor))
  (make-object CmapProcessor (· this cmap)))


;; Returns a Subset for this font.
(define/contract (createSubset this)
  (->m (is-a?/c Subset))
  (make-object (if (· this has-cff-table?)
                   CFFSubset
                   TTFSubset) this))



(define (has-table? this tag)
  (FT_Load_Sfnt_Table (· this ft-face) (tag->int tag) 0 0 0))
  
(define (has-cff-table? this) (has-table? this #"CFF "))

(define (has-morx-table? this) (has-table? this #"morx"))

(define (has-gpos-table? this) (has-table? this #"GPOS"))

(define (has-gsub-table? this) (has-table? this #"GSUB"))


;; Returns a glyph object for the given glyph id.
;; You can pass the array of code points this glyph represents for
;; your use later, and it will be stored in the glyph object.
(define/contract (getGlyph this glyph [characters null])
  ((index?) ((listof index?)) . ->*m . (is-a?/c Glyph))
  (make-object (if (· this has-cff-table?)
                   CFFGlyph
                   TTFGlyph) glyph characters this))


;; Returns a GlyphRun object, which includes an array of Glyphs and GlyphPositions for the given string.
(define/contract (layout this string [userFeatures #f] [script #f] [language #f])
  ((string?) ((or/c (listof symbol?) #f) (or/c symbol? #f) (or/c symbol? #f)) . ->*m . (is-a?/c GlyphRun))
  (unless (· this _layoutEngine)
    (set-field! _layoutEngine this (make-object LayoutEngine this)))
  (send (· this _layoutEngine) layout string userFeatures script language))


;; Returns an array of Glyph objects for the given string.
;; This is only a one-to-one mapping from characters to glyphs.
;; For most uses, you should use font.layout (described below), which
;; provides a much more advanced mapping supporting AAT and OpenType shaping.
(define/contract (glyphsForString this string)
  (string? . ->m . (listof (is-a?/c Glyph)))

  ;; todo: make this handle UTF-16 with surrogate bytes
  ;; for now, just use UTF-8
  (define codepoints (map char->integer (string->list string)))
  (for/list ([cp (in-list codepoints)])
    (send this glyphForCodePoint cp)))


;; Maps a single unicode code point to a Glyph object.
;; Does not perform any advanced substitutions (there is no context to do so).
(define/contract (glyphForCodePoint this codePoint)
  (index? . ->m . (is-a?/c Glyph))
  (define glyph-idx (FT_Get_Char_Index (· this ft-face) codePoint))
  (send this getGlyph glyph-idx (list codePoint)))


(define/contract (measure-char-width this char)
  (char? . ->m . number?)
  (define glyph-idx (FT_Get_Char_Index (· this ft-face) (char->integer char)))
  (FT_Load_Glyph (· this ft-face) glyph-idx FT_LOAD_NO_RECURSE)
  (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph (· this ft-face)))))
  (* width 1.0))
  

(define/contract (measure-string this str size)
  (string? number? . ->m . number?)
  (/ (* size
        (for/sum ([c (in-string str)])
          (measure-char-width this c))) (· this unitsPerEm)))


;; Register font formats
(define formats (list TTFFont))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo


(define/contract (create filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . any/c)
  (or
   (for*/first ([format (in-list formats)]
                [font (in-value (make-object format filename))]
                #:when font)
     (if postscriptName
         (send font getFont postscriptName) ; used to select from collection files like TTC
         font))
   (error 'create "unknown font format")))


(define/contract (openSync filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . any/c)
  (create filename postscriptName))


(module+ test
  (require rackunit)
  (define f (openSync (path->string charter-path)))
  (check-equal? (postscriptName f) "Charter")
  (check-equal? (· f unitsPerEm) 1000)
  (check-equal? (· f ascent) 980)
  (check-equal? (· f descent) -238)
  (check-equal? (measure-string f "f" (· f unitsPerEm)) 321.0)
  (check-false (· f has-cff-table?))
  (check-false (· f has-morx-table?))
  (check-false (· f has-gsub-table?))
  (check-false (· f has-gpos-table?))
  (check-true (send f has-table? #"cmap"))
  (check-equal? (· f lineGap) 0)
  #;(· f createSubset)
  

  )