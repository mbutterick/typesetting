#lang debug racket/base
(require (for-syntax racket/base)
         "helper.rkt"
         "ffi/freetype.rkt"
         "subset.rkt"
         "glyph.rkt"
         "bbox.rkt"
         "glyphrun.rkt"
         "directory.rkt"
         xenomorph
         "tables.rkt"
         racket/contract
         racket/class
         racket/match
         racket/file
         sugar/unstable/class
         sugar/unstable/contract
         sugar/unstable/dict
         sugar/unstable/js
         racket/port
         "ffi/harfbuzz.rkt"
         "glyph-position.rkt"
         sugar/list
         racket/promise)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/TTFFont.js
|#

(require (for-syntax "tables.rkt"))
(define-syntax (define-table-getters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(TABLE-TAG ...) (hash-keys table-codecs)])
       #'(begin
           (define/public (TABLE-TAG) (_getTable 'TABLE-TAG)) ...))]))


(test-module
 (define f (openSync (path->string charter-path)))
 (define otf (openSync (path->string fira-otf-path)))
 (check-equal? (postscriptName f) "Charter"))

;; This is the base class for all SFNT-based font formats in fontkit.
;; (including CFF)
;;  It supports TrueType, and PostScript glyphs, and several color glyph formats.

(define ft-library (delay (FT_Init_FreeType)))

(define-subclass object% (TTFFont port [_src #f])
  (when port (unless (input-port? port)
               (raise-argument-error 'TTFFont "input port" port)))
  (unless (member (peek-bytes 4 0 port) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (raise 'probe-fail))
  
  ;; skip variationCoords
  (field [_decoded-tables (mhash)]
         [_port (open-input-bytes (port->bytes port))]
         [_directory (delay (decode Directory _port #:parent (mhash '_startOffset 0)))]
         [_ft-face (delay (and _src (FT_New_Face (force ft-library) _src)))]
         [_hb-font (delay (and _src (hb_ft_font_create (· this ft-face))))]
         [_hb-buf (delay (hb_buffer_create))])
 
  (define/public (directory) (force _directory))
  (define/public (ft-face) (or (force _ft-face) (error 'ft-face-not-available)))
  (define/public (hb-font) (or (force _hb-font) (error 'hb-font-not-available)))
  (define/public (hb-buf) (force _hb-buf))

  (define/public (_getTable table-tag)
    (unless (has-table? this table-tag)
      (raise-argument-error '_getTable "table that exists in font" table-tag))
    (dict-ref! _decoded-tables table-tag (λ () (_decodeTable table-tag))))

  (define-table-getters)

  (define/public (_getTableStream tag)
    (define table (hash-ref (· this directory tables) tag))
    (and table (pos _port (· table offset)) _port))
  
  (define/public (_decodeTable table-tag)
    (unless (hash-has-key? table-codecs table-tag)
      (raise-argument-error '_decodeTable "decodable table" table-tag))
    (pos _port 0)
    (define table (hash-ref (· this directory tables) table-tag))
    (define table-bytes (open-input-bytes (peek-bytes (· table length) (· table offset) _port)))
    (define table-decoder (hash-ref table-codecs table-tag))
    (decode table-decoder table-bytes #:parent this))

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
  (->m BBox?)
  (make-BBox (· this head xMin)
             (· this head yMin)
             (· this head xMax)
             (· this head yMax)))

(test-module
 (check-equal? (bbox->list (· f bbox)) '(-161 -236 1193 963)))

;; Returns a Subset for this font.
(define/contract (createSubset this)
  (->m (is-a?/c Subset))
  ;; no CFF support
  #;(make-object (if (· this has-cff-table?)
                     CFFSubset
                     TTFSubset) this)
  (make-object TTFSubset this))



(define/contract (has-table? this tag)
  ((or/c bytes? symbol?) . ->m . boolean?)
  (dict-has-key? (· this directory tables) (if (bytes? tag)
                                               (string->symbol (bytes->string/latin-1 tag))
                                               tag)))
  
(define (has-cff-table? x) (has-table? x 'CFF_))
(define (has-morx-table? x) (has-table? x 'morx))
(define (has-gpos-table? x) (has-table? x 'GPOS))
(define (has-gsub-table? x) (has-table? x 'GSUB))

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

(define current-layout-caching (make-parameter #false))
(define layout-cache (make-hash))

(define (harfbuzz-glyphrun this string userFeatures script language)
  #;(string? (listof symbol?) symbol? symbol? . ->m . GlyphRun?)
  (define buf (· this hb-buf))
  (hb_buffer_reset buf)
  (hb_buffer_add_codepoints buf (map char->integer (string->list string)))
  (define chars (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf)))
  (hb_shape (· this hb-font) buf (map tag->hb-feature (or userFeatures null)))
  (define-values (gidxs clusters)
    (for/lists (gs cs)
               ([gi (in-list (hb_buffer_get_glyph_infos buf))])
      (values (hb_glyph_info_t-codepoint gi) (hb_glyph_info_t-cluster gi))))
  (define glyphs (for/list ([gidx (in-list gidxs)]
                            [char-cluster (in-list (break-at chars clusters))])
                           (send this getGlyph gidx char-cluster)))
  (define positions (for/list ([gp (in-list (hb_buffer_get_glyph_positions buf))])
                              (match (hb_glyph_position_t->list gp)
                                [(list xad yad xoff yoff _) (+GlyphPosition xad yad xoff yoff)])))
  (+GlyphRun glyphs positions))

;; Returns a GlyphRun object, which includes an array of Glyphs and GlyphPositions for the given string.
(define/contract (layout this string [userFeatures #f] [script #f] [language #f])
  ((string?) ((option/c (listof symbol?)) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  (define (get-layout string)
    (define key (list string (and userFeatures (sort userFeatures symbol<?)) script language))
    (hash-ref! layout-cache key (λ () (apply harfbuzz-glyphrun this key))))
  ;; work on substrs to reuse cached pieces
  ;; caveat: no shaping / positioning that involve word spaces
  (cond
    ;; todo: why does caching produce slightly different results in test files
    ;; theory: because word space is not included in shaping
    [(current-layout-caching)
     (define substrs (for/list ([substr (in-list (regexp-match* " " string #:gap-select? #t))]
                                #:when (positive? (string-length substr)))
                               substr))
     (apply append-glyphruns (map get-layout substrs))]
    [else (get-layout string)]))


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
                [font (in-value (with-handlers ([(λ (x) (eq? x 'probe-fail)) (λ (exn) #f)])
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

