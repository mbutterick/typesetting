#lang debug fontkit/racket
(require "freetype-ffi.rkt" (except-in ffi/unsafe array?) racket/runtime-path "subset.rkt" "glyph.rkt" "layout-engine.rkt" "bbox.rkt" "glyphrun.rkt" "cmap-processor.rkt" "directory.rkt" xenomorph "tables.rkt" "ttfglyph.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/TTFFont.js
|#

(require (for-syntax "tables.rkt"))
(define-macro (define-table-getters)
  (with-pattern ([(TABLE-TAG ...) (hash-keys table-codecs)])
                #'(begin
                    (define/public (TABLE-TAG) (_getTable 'TABLE-TAG)) ...)))


(test-module
 (define f (openSync (path->string charter-path)))
 (define otf (openSync (path->string fira-otf-path)))
 (check-equal? (postscriptName f) "Charter"))

;; This is the base class for all SFNT-based font formats in fontkit.
;; (including CFF)
;;  It supports TrueType, and PostScript glyphs, and several color glyph formats.
(define-subclass object% (TTFFont port [_src #f])
  (when port (unless (input-port? port)
               (raise-argument-error 'TTFFont "input port" port)))
  (unless (member (peek-bytes 4 0 port) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (raise 'probe-fail))
  
  ;; skip variationCoords
  (field [_directoryPos (pos port)]
         [_tables (mhash)] ; holds decoded tables (loaded lazily)
         [_glyphs (mhash)]
         [_layoutEngine #f])

  (field [directory #f])
  (send this _decodeDirectory)

  (define/public (_getTable table-tag)
    (unless (has-table? this table-tag)
      (raise-argument-error '_getTable "table that exists in font" table-tag))
    (dict-ref! _tables table-tag (_decodeTable table-tag))) ; get table from cache, load if not there

  (define-table-getters)

  (define/public (_getTableStream tag)
    (define table (dict-ref (· this directory tables) tag))
    (cond
      [table
       (pos port (· table offset))
       port]
      [else #f]))

  (define/public (_decodeTable table-tag)
    (define table-decoder (hash-ref table-codecs table-tag
                                    (λ () (raise-argument-error '_decodeTable "decodable table" table-tag))))
    (define offset (· (hash-ref (· directory tables) table-tag) offset))
    (define len (· (hash-ref (· directory tables) table-tag) length))
    (pos port 0)
    (decode table-decoder (open-input-bytes (peek-bytes len offset port)) #:parent this))

  (define/public (_decodeDirectory)
    (set! directory (decode Directory port #:parent (mhash '_startOffset 0)))
    directory)

  (field [ft-library (FT_Init_FreeType)]
         [ft-face (and _src (FT_New_Face ft-library _src 0))])

  (as-methods
   postscriptName
   measure-string
   unitsPerEm
   ascent
   descent
   lineGap
   underlinePosition
   underlineThickness
   italicAngle
   capHeight
   xHeight
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
  (· this head unitsPerEm))

(test-module
 (check-equal? (· f unitsPerEm) 1000))

;; The font’s [ascender](https://en.wikipedia.org/wiki/Ascender_(typography))
(define/contract (ascent this)
  (->m number?)
  (· this hhea ascent))

(test-module
 (check-equal? (· f ascent) 980))


;; The font’s [descender](https://en.wikipedia.org/wiki/Descender)
(define/contract (descent this)
  (->m number?)
  (· this hhea descent))

(test-module
 (check-equal? (· f descent) -238))

;; The amount of space that should be included between lines
(define/contract (lineGap this)
  (->m number?)
  (· this hhea lineGap))

(test-module
 (check-equal? (· f lineGap) 0))


(define/contract (underlinePosition this)
  (->m number?)
  (· this post underlinePosition))

(test-module
 (check-equal? (· f underlinePosition) -178))


(define/contract (underlineThickness this)
  (->m number?)
  (· this post underlineThickness))

(test-module
 (check-equal? (· f underlineThickness) 58))


;; If this is an italic font, the angle the cursor should be drawn at to match the font design
(define/contract (italicAngle this)
  (->m number?)
  (· this post italicAngle))

(test-module
 (check-equal? (· f italicAngle) 0))


;; The height of capital letters above the baseline.
(define/contract (capHeight this)
  (->m number?)
  (if (send this has-table? #"OS/2")
      (· this OS/2 capHeight)
      (· this ascent)))

(test-module
 (check-equal? (· f capHeight) 671))


;; The height of lower case letters in the font.
(define/contract (xHeight this)
  (->m number?)
  (if (send this has-table? #"OS/2")
      (· this OS/2 xHeight)
      0))

(test-module
 (check-equal? (· f xHeight) 481))


;; The font’s bounding box, i.e. the box that encloses all glyphs in the font.
(define/contract (bbox this)
  (->m (is-a?/c BBox))
  (make-object BBox (· this head xMin)
    (· this head yMin)
    (· this head xMax)
    (· this head yMax)))

(test-module
 (check-equal? (bbox->list (· f bbox)) '(-161 -236 1193 963)))


(define/contract (_cmapProcessor this)
  (->m (is-a?/c CmapProcessor))
  (make-object CmapProcessor (· this cmap)))


;; Returns a Subset for this font.
(define/contract (createSubset this)
  (->m (is-a?/c Subset))
  (make-object (if (· this has-cff-table?)
                   CFFSubset
                   TTFSubset) this))



(define/contract (has-table? this tag)
  ((or/c bytes? symbol?) . ->m . boolean?)
  (dict-has-key? (· this directory tables) (if (bytes? tag)
                                               (string->symbol (bytes->string/latin-1 tag))
                                               tag)))
  
(define has-cff-table? (curryr has-table? 'CFF_))
(define has-morx-table? (curryr has-table? 'morx))
(define has-gpos-table? (curryr has-table? 'GPOS))
(define has-gsub-table? (curryr has-table? 'GSUB))

(test-module
 (check-false (· f has-cff-table?))
 (check-false (· f has-morx-table?))
 (check-false (· f has-gsub-table?))
 (check-false (· f has-gpos-table?)))


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
  ((string?) ((option/c (listof symbol?)) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  (unless (· this _layoutEngine)
    (set-field! _layoutEngine this (+LayoutEngine this)))
  #;(report*/file 'in-layout (· this _layoutEngine))
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
  (index? . ->m . Glyph?)
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


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/index.js
|#

;; Register font formats
(define formats (list TTFFont))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/base.js
|#

(define/contract (openSync str-or-path [postscriptName #f])
  (((or/c path? string?)) ((option/c string?)) . ->* . TTFFont?)
  (define filename (if (path? str-or-path) (path->string str-or-path) str-or-path))
  (define buffer (file->bytes filename))
  (create buffer filename postscriptName))



(define/contract (create buffer [filename #f] [postscriptName #f])
  ((bytes?) ((option/c path-string?) (option/c string?)) . ->* . TTFFont?)
  (or
   (for*/first ([format (in-list formats)]
                ;; rather than use a `probe` function,
                ;; just try making a font with each format and see what happens
                [font (in-value (with-handlers ([(curry eq? 'probe-fail) (λ (exn) #f)])
                                  (make-object format (open-input-bytes buffer) filename)))]
                #:when font)
               (if postscriptName
                   (send font getFont postscriptName) ; used to select from collection files like TTC
                   font))
   (error 'fontkit:create "unknown font format")))


(test-module
 (check-equal? (measure-string f "f" (· f unitsPerEm)) 321.0)
 (check-true (send f has-table? #"cmap"))
 (check-exn exn:fail:contract? (λ () (send f _getTable 'nonexistent-table-tag))))

