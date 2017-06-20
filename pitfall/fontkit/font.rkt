#lang fontkit/racket
(require "freetype-ffi.rkt" ffi/unsafe racket/runtime-path "subset.rkt" "glyph.rkt" "layout-engine.rkt" "bbox.rkt" "glyphrun.rkt" "cmap-processor.rkt" "directory.rkt" restructure "tables.rkt" "ttfglyph.rkt")
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

;; This is the base class for all SFNT-based font formats in fontkit.
;;  It supports TrueType, and PostScript glyphs, and several color glyph formats.
(define-subclass object% (TTFFont stream [_src #f])
  (when stream (unless (DecodeStream? stream)
                 (raise-argument-error 'TTFFont "DecodeStream" stream)))
  (unless (member (peek-bytes 4 0 (get-field _port  stream)) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (raise 'probe-fail))
  
  ;; skip variationCoords
  (field [_directoryPos (send stream pos)]
         [_tables (mhash)] ; holds decoded tables (loaded lazily)
         [_glyphs (mhash)]
         [_layoutEngine #f])

  (field [directory #f])
  (send this _decodeDirectory)

  (define/public (_getTable table-tag)
    (unless (has-table? this table-tag)
      (raise-argument-error '_getTable "table that exists in font" table-tag))
    (hash-ref! _tables table-tag (_decodeTable table-tag))) ; get table from cache, load if not there

  (define-table-getters)

  (define/public (_getTableStream tag)
    (define table (hash-ref (· this directory tables) tag))
    (cond
      [table
       (send stream pos (· table offset))
       stream]
      [else #f]))

  (define/public (_decodeTable table-tag)
    (define table-decoder (hash-ref table-codecs table-tag
                                    (λ () (raise-argument-error '_decodeTable "decodable table" table-tag))))
    (define offset (· (hash-ref (· directory tables) table-tag) offset))
    (define len (· (hash-ref (· directory tables) table-tag) length))
    (send stream pos 0)
    (send table-decoder decode (+DecodeStream (peek-bytes len offset (get-field _port stream))) this length))

  (define/public (_decodeDirectory)
    (set! directory (send Directory decode stream (mhash '_startOffset 0)))
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


;; The font’s [ascender](https://en.wikipedia.org/wiki/Ascender_(typography))
(define/contract (ascent this)
  (->m number?)
  (· this hhea ascent))


;; The font’s [descender](https://en.wikipedia.org/wiki/Descender)
(define/contract (descent this)
  (->m number?)
  (· this hhea descent))


;; The amount of space that should be included between lines
(define/contract (lineGap this)
  (->m number?)
  (· this hhea lineGap))


(define/contract (underlinePosition this)
  (->m number?)
  (define post-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_post) _pointer _FT_TT_Postscript-pointer))
  (FT_TT_Postscript-underlinePosition post-table))



(define/contract (underlineThickness this)
  (->m number?)
  (define post-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_post) _pointer _FT_TT_Postscript-pointer))
  (FT_TT_Postscript-underlineThickness post-table))


;; If this is an italic font, the angle the cursor should be drawn at to match the font design
(define/contract (italicAngle this)
  (->m number?)
  (define post-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_post) _pointer _FT_TT_Postscript-pointer))
  (FT_TT_Postscript-italicAngle post-table))


;; The height of capital letters above the baseline.
(define/contract (capHeight this)
  (->m number?)
  (cond
    [(send this has-table? #"OS/2")
     (define os2-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_os2) _pointer _FT_TT_OS2-pointer))
     (FT_TT_OS2-sCapHeight os2-table)]
    [else (· this ascent)]))


;; The height of lower case letters in the font.
(define/contract (xHeight this)
  (->m number?)
  (cond
    [(send this has-table? #"OS/2")
     (define os2-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_os2) _pointer _FT_TT_OS2-pointer))
     (FT_TT_OS2-sxHeight os2-table)]
    [else 0]))


;; The font’s bounding box, i.e. the box that encloses all glyphs in the font.
(define/contract (bbox this)
  (->m (is-a?/c BBox))
  (make-object BBox (· this head xMin)
    (· this head yMin)
    (· this head xMax)
    (· this head yMax)))


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
  (hash-has-key? (· this directory tables) (if (bytes? tag)
                                               (string->symbol (bytes->string/latin-1 tag))
                                               tag)))
  
(define has-cff-table? (curryr has-table? '|CFF |))
(define has-morx-table? (curryr has-table? 'morx))
(define has-gpos-table? (curryr has-table? 'GPOS))
(define has-gsub-table? (curryr has-table? 'GSUB))


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
  #;(FT_Select_Charmap (· this ft-face) (tag->int #"unic"))
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



(define/contract (openSync filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . TTFFont?)
  (define buffer (file->bytes filename))
  (create buffer filename postscriptName))



(define/contract (create buffer [filename #f] [postscriptName #f])
  ((bytes?) ((or/c path-string? #f) (or/c string? #f)) . ->* . TTFFont?)
  (or
   (for*/first ([format (in-list formats)]
                ;; rather than use a `probe` function,
                ;; just try making a font with each format and see what happens
                [font (in-value (with-handlers ([(curry eq? 'probe-fail) (λ (exn) #f)])
                                  (make-object format (+DecodeStream buffer) filename)))]
                #:when font)
               (if postscriptName
                   (send font getFont postscriptName) ; used to select from collection files like TTC
                   font))
   (error 'fontkit:create "unknown font format")))


(test-module
 (define f (openSync (path->string charter-path)))
 (check-equal? (postscriptName f) "Charter")
 (check-equal? (· f unitsPerEm) 1000)
 (check-equal? (· f ascent) 980)
 (check-equal? (· f descent) -238)
 (check-equal? (bbox->list (· f bbox)) '(-161 -236 1193 963))
 (check-equal? (measure-string f "f" (· f unitsPerEm)) 321.0)
 (check-false (· f has-cff-table?))
 (check-false (· f has-morx-table?))
 (check-false (· f has-gsub-table?))
 (check-false (· f has-gpos-table?))
 (check-true (send f has-table? #"cmap"))
 (check-equal? (· f lineGap) 0)
 (check-exn exn:fail:contract? (λ () (send f _getTable 'nonexistent-table-tag)))
 #;(send f _getTable 'maxp)
 (define subset (make-object TTFSubset f))
 (define es (+EncodeStream))
 (send subset encode es)
 #;(with-output-to-file "subsetfont.rktd" (λ () (display (send es dump)) ))
 #;(check-equal? (send es dump) (file->bytes "subsetfont.rktd"))

 (file-directory-decode "subsetfont.rktd")
 (file-directory-decode "../pitfall/test/out.bin")
 )