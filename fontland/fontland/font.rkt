#lang debug racket/base
(require (for-syntax racket/base)
         "helper.rkt"
         "unsafe/freetype.rkt"
         "subset.rkt"
         "glyph.rkt"
         "bbox.rkt"
         "glyphrun.rkt"
         "directory.rkt"
         "db.rkt"
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
         "unsafe/harfbuzz.rkt"
         "glyph-position.rkt"
         sugar/list
         racket/promise
         crc32c)
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
 (define fira (openSync (path->string fira-path)))
 (define otf (openSync (path->string fira-otf-path)))
 (check-equal? (postscriptName f) "Charter"))

;; This is the base class for all SFNT-based font formats in fontkit.
;; (including CFF)
;;  It supports TrueType, and PostScript glyphs, and several color glyph formats.

(define ft-library (delay (FT_Init_FreeType)))

(define-subclass object% (TTFFont _port)
  (unless (input-port? _port)
    (raise-argument-error 'TTFFont "input port" _port))
  (unless (member (peek-bytes 4 0 _port) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (raise 'probe-fail))
  
  ;; skip variationCoords
  (field [_decoded-tables (mhash)]
         [_src (path->string (object-name _port))]
         [_directory (delay (decode Directory _port #:parent (mhash '_startOffset 0)))]
         [_ft-face (delay (and _src (FT_New_Face (force ft-library) _src)))]
         [_hb-font (delay (and _src (hb_ft_font_create (· this ft-face))))]
         [_hb-buf (delay (hb_buffer_create))]
         [_crc (begin0 (crc32c-input-port _port) (pos _port 0))])
 
  (define/public (directory) (force _directory))
  (define/public (ft-face) (or (force _ft-face) (error 'ft-face-not-available)))
  (define/public (hb-font) (or (force _hb-font) (error 'hb-font-not-available)))
  (define/public (hb-buf) (force _hb-buf))

  (define/public (_getTable table-tag)
    (unless (has-table? this table-tag)
      (raise-argument-error '_getTable "table that exists in font" table-tag))
    (hash-ref! _decoded-tables table-tag (λ () (_decodeTable table-tag))))

  (define-table-getters)

  (define/public (_getTableStream tag)
    (define table (hash-ref (· this directory tables) tag))
    (and table (pos _port (· table offset)) _port))
  
  (define/public (_decodeTable table-tag)
    (unless (hash-has-key? table-codecs table-tag)
      (raise-argument-error '_decodeTable "decodable table" table-tag))
    (define table (hash-ref (· this directory tables) table-tag))
    ;; todo: possible to avoid copying the bytes here?
    (pos _port (· table offset))
    (define table-bytes (open-input-bytes (peek-bytes (· table length) 0 _port)))
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
  (make-BBox (· this head xMin) (· this head yMin) (· this head xMax) (· this head yMax)))

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
  (hash-has-key? (· this directory tables) (match tag
                                             [(? bytes?) (string->symbol (bytes->string/latin-1 tag))]
                                             [_ tag])))
  
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
  ;; no CFF
  #;(make-object (if (· this has-cff-table?)
                     CFFGlyph
                     TTFGlyph) glyph characters this)
  (make-object TTFGlyph glyph characters this))

(define current-layout-caching (make-parameter #false))

(struct hb-gid (val) #:transparent)
(struct hb-cluster (chars) #:transparent)
(struct hb-position (xad yad xoff yoff etc) #:transparent)
(struct hb-layout (hb-gids hb-clusters hb-positions) #:transparent)

(define hb-output (+Struct (dictify
                            'hb-gids (+Array uint16 uint16)
                            'hb-clusters (+Array (+Array uint16 uint16) uint16)
                            'hb-positions (+Array (+Array uint16 5) uint16))))

(define (hb-layout->glyphrun this hbr)
  (match hbr
    [(hash-table ('hb-gids gidxs)
                 ('hb-clusters clusters)
                 ('hb-positions posns))
     (define glyphs (for/list ([gidx (in-list gidxs)]
                               [cluster (in-list clusters)])
                              (send this getGlyph gidx cluster)))
     (define positions (for/list ([pos (in-list posns)])
                                 (match pos
                                   [(list xad yad xoff yoff _) (+GlyphPosition xad yad xoff yoff)])))
     (+GlyphRun glyphs positions)]))


(define (harfbuzz-layout this codepoints userFeatures script language)
  #;(string? (listof symbol?) symbol? symbol? . ->m . GlyphRun?)
  (define buf (· this hb-buf))
  (hb_buffer_reset buf)
  (hb_buffer_add_codepoints buf codepoints)
  (define chars (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf)))
  (hb_shape (· this hb-font) buf (map tag->hb-feature (or userFeatures null)))
  (define gis (hb_buffer_get_glyph_infos buf))
  (dictify 'hb-gids (map hb_glyph_info_t-codepoint gis)
           'hb-clusters (break-at chars (map hb_glyph_info_t-cluster gis))
           'hb-positions (map hb_glyph_position_t->list (hb_buffer_get_glyph_positions buf))))


(define layout-cache (make-hasheqv))

(require xenomorph/struct)
(define hb-input (+Struct (dictify
                           'font-crc uint32
                           'codepoints (+Array uint16)
                           'userFeatures (+Array (+String uint8)))))

(define (layout-cache-key font-crc codepoints user-features . _)
  (crc32c-bytes (encode hb-input (dictify 
                                  'font-crc font-crc
                                  'codepoints codepoints
                                  'userFeatures user-features) #f)))


;; Returns a GlyphRun object, which includes an array of Glyphs and GlyphPositions for the given string.
(define (layout this string [userFeatures #f] [script #f] [language #f] #:debug [debug #f])
  #;((string?) ((option/c (listof symbol?)) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  (define (get-layout string)
    (define codepoints (map char->integer (string->list string)))
    (define args (list codepoints (if userFeatures (sort userFeatures symbol<?) null) script language))
    (define key (apply layout-cache-key (· this _crc) args))
    (hash-ref! layout-cache key
               (λ ()
                 #;(encode hb-output (apply harfbuzz-layout this args) #f)
                 (match (get-layout-from-db key)
                   [(? bytes? res) (dump (decode hb-output res))]
                   [_  (define new-layout (apply harfbuzz-layout this args))
                       (add-record! (cons key (encode hb-output new-layout #f)))
                       (make-hasheq new-layout)])))) ;; `dump` converts to hash
  ;; work on substrs to reuse cached pieces
  ;; caveat: no shaping / positioning that involve word spaces
  ;; todo: why does caching produce slightly different results in test files
  ;; theory: because word space is not included in shaping
  (cond
    [(current-layout-caching)
     (define substrs (for/list ([substr (in-list (regexp-match* " " string #:gap-select? #t))]
                                #:when (positive? (string-length substr)))
                               substr))
     (apply append-glyphruns (map (λ (lo) (hb-layout->glyphrun this lo)) (map get-layout substrs)))]
    [else (if debug
              (get-layout string)
              (hb-layout->glyphrun this (get-layout string)))]))


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
(define font-formats (list TTFFont))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/base.js
|#

(define/contract (openSync str-or-path)
  ((or/c path? string?) . -> . TTFFont?)
  (define filename (if (path? str-or-path) (path->string str-or-path) str-or-path))
  (create (open-input-file filename)))

(define (probe-failed? x) (eq? x 'probe-fail))

(define/contract (create port)
  (input-port? . -> . TTFFont?)
  (or
   ;; rather than use a `probe` function,
   ;; just try making a font with each format and see what happens
   (for/first ([font-format (in-list font-formats)])
              (with-handlers ([probe-failed? (λ (exn) #f)])
                (make-object font-format port)))
   (error 'fontland:create "unknown font format")))


(test-module
 (check-equal? (measure-string f "f" (· f unitsPerEm)) 321.0)
 (check-true (send f has-table? #"cmap"))
 (check-exn exn:fail:contract? (λ () (send f _getTable 'nonexistent-table-tag)))
 (check-true
  (let ([h (layout fira "Rifle" #:debug #t)])
    (and (equal? (hash-ref h 'hb-gids) '(227 480 732 412))
         (equal? (hash-ref h 'hb-clusters) '((82) (105) (102 108) (101)))
         (equal? (hash-ref h 'hb-positions) '((601 0 0 0 0) (279 0 0 0 0) (580 0 0 0 0) (547 0 0 0 0)))))))
