#lang debug racket/base
(require (for-syntax racket/base)
         "helper.rkt"
         "unsafe/freetype.rkt"
         "subset.rkt"
         "glyph.rkt"
         "ttf-glyph.rkt"
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




(test-module
 (define f (openSync (path->string charter-path)))
 (define fira (openSync (path->string fira-path)))
 (define otf (openSync (path->string fira-otf-path)))
 (check-equal? (postscriptName f) "Charter"))

;; This is the base class for all SFNT-based font formats in fontkit.
;; (including CFF)
;;  It supports TrueType, and PostScript glyphs, and several color glyph formats.

(require "struct.rkt")

(define ft-library (delay (FT_Init_FreeType)))

(define (+ttf-font _port [_decoded-tables (mhash)]
                  [_src (path->string (object-name _port))]
                  [_directory (delay (decode Directory _port #:parent (mhash '_startOffset 0)))]
                  [_ft-face (delay (and _src (FT_New_Face (force ft-library) _src)))]
                  [_hb-font (delay (and _src (hb_ft_font_create (force _ft-face))))]
                  [_hb-buf (delay (hb_buffer_create))]
                  [_crc (begin0 (crc32c-input-port _port) (pos _port 0))]
                  [_get-head-table #f])
  (unless (input-port? _port)
    (raise-argument-error '+ttf-font "input port" _port))
  (unless (member (peek-bytes 4 0 _port) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (raise 'probe-fail))
  (define f
    (ttf-font _port _decoded-tables _src _directory _ft-face _hb-font _hb-buf _crc _get-head-table))
  
    ;; needed for `loca` table decoding cross-reference
  (set-ttf-font-get-head-table-proc! f (λ () (get-head-table f)))
  f)


(define (directory this) (force (· this _directory)))
(define (hb-font this) (or (force (ttf-font-hb-font this)) (error 'hb-font-not-available)))
(define (hb-buf this) (force (ttf-font-hb-buf this)))

(require "table-stream.rkt")

;; The unique PostScript name for this font
(define (postscriptName this)
  (FT_Get_Postscript_Name (ft-face this)))


;; The size of the font’s internal coordinate grid
(define (unitsPerEm this)
  (· (get-head-table this) unitsPerEm))

(test-module
 (check-equal? (unitsPerEm f) 1000))

;; The font’s [ascender](https://en.wikipedia.org/wiki/Ascender_(typography))
(define (ascent this)
  (· (get-hhea-table this) ascent))

(test-module
 (check-equal? (ascent f) 980))


;; The font’s [descender](https://en.wikipedia.org/wiki/Descender)
(define (descent this)
  (· (get-hhea-table this) descent))

(test-module
 (check-equal? (descent f) -238))

;; The amount of space that should be included between lines
(define (lineGap this)
  (· (get-hhea-table this) lineGap))
(define line-gap lineGap) ; todo: avoid this name collision in pitfall/embedded

(test-module
 (check-equal? (lineGap f) 0))


(define (underlinePosition this)
  (· (get-post-table this) underlinePosition))

(test-module
 (check-equal? (underlinePosition f) -178))


(define (underlineThickness this)
  (· (get-post-table this) underlineThickness))

(test-module
 (check-equal? (underlineThickness f) 58))


;; If this is an italic font, the angle the cursor should be drawn at to match the font design
(define (italicAngle this)
  (· (get-post-table this) italicAngle))

(test-module
 (check-equal? (italicAngle f) 0))


;; The height of capital letters above the baseline.
(define (capHeight this)
  (if (has-table? this #"OS/2")
      (· (get-OS/2-table this) capHeight)
      (ascent this)))

(test-module
 (check-equal? (capHeight f) 671))


;; The height of lower case letters in the font.
(define (xHeight this)
  (if (has-table? this #"OS/2")
      (· (get-OS/2-table this) xHeight)
      0))

(test-module
 (check-equal? (xHeight f) 481))


;; The font’s bounding box, i.e. the box that encloses all glyphs in the font.
(define (bbox this)
  (define head-table (get-head-table this))
  (make-BBox (· head-table xMin) (· head-table yMin) (· head-table xMax) (· head-table yMax)))

(define font-bbox bbox) ; todo: avoid name collision in pitfall/embedded

(test-module
 (check-equal? (bbox->list (bbox f)) '(-161 -236 1193 963)))


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
                      (get-glyph this gidx cluster)))
     (define positions (for/list ([pos (in-list posns)])
                         (match pos
                           [(list xad yad xoff yoff _) (+glyph-position xad yad xoff yoff)])))
     (glyphrun glyphs positions)]))


(define (harfbuzz-layout this codepoints userFeatures script language)
  #;(string? (listof symbol?) symbol? symbol? . ->m . GlyphRun?)
  (define buf (hb-buf this))
  (hb_buffer_reset buf)
  (hb_buffer_add_codepoints buf codepoints)
  (define chars (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf)))
  (hb_shape (hb-font this) buf (map tag->hb-feature (or userFeatures null)))
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
(define (layout this string [user-features #f] [script #f] [language #f] #:debug [debug #f])
  #;((string?) ((option/c (listof symbol?)) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  (define (get-layout string)
    (define codepoints (map char->integer (string->list string)))
    (define args (list codepoints (if user-features (sort user-features symbol<?) null) script language))
    (define key (apply layout-cache-key (ttf-font-crc this) args))
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
(define (glyphsForString this string)
  #;(string? . ->m . (listof glyph?))

  ;; todo: make this handle UTF-16 with surrogate bytes
  ;; for now, just use UTF-8
  (define codepoints (map char->integer (string->list string)))
  (for/list ([cp (in-list codepoints)])
    (send this glyphForCodePoint cp)))


;; Maps a single unicode code point to a Glyph object.
;; Does not perform any advanced substitutions (there is no context to do so).
(define (glyph-for-codepoint this codepoint)
  (define glyph-idx (FT_Get_Char_Index (· this ft-face) codepoint))
  (get-glyph this glyph-idx (list codepoint)))


(define (measure-char-width this char)
  (define glyph-idx (FT_Get_Char_Index (ft-face this) (char->integer char)))
  (FT_Load_Glyph (ft-face this) glyph-idx FT_LOAD_NO_RECURSE)
  (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph (ft-face this)))))
  (* width 1.0))
  

(define (measure-string this str size)
  (/ (* size
        (for/sum ([c (in-string str)])
          (measure-char-width this c))) (unitsPerEm this)))


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/index.js
|#

;; Register font formats
(define font-formats (list +ttf-font))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/base.js
|#

(define (openSync str-or-path)
  (define filename (if (path? str-or-path) (path->string str-or-path) str-or-path))
  (create (open-input-file filename)))

(define (probe-failed? x) (eq? x 'probe-fail))

(define (create port)
  (or
   ;; rather than use a `probe` function,
   ;; just try making a font with each format and see what happens
   (for/first ([font-format (in-list font-formats)])
     (with-handlers ([probe-failed? (λ (exn) #f)])
       (font-format port)))
   (error 'fontland:create "unknown font format")))


(test-module
 (check-equal? (measure-string f "f" (unitsPerEm f)) 321.0)
 (check-true (has-table? f #"cmap"))
 (check-exn exn:fail:contract? (λ () (get-table f 'nonexistent-table-tag)))
 (check-true
  (let ([h (layout fira "Rifle" #:debug #t)])
    (and (equal? (hash-ref h 'hb-gids) '(227 480 732 412))
         (equal? (hash-ref h 'hb-clusters) '((82) (105) (102 108) (101)))
         (equal? (hash-ref h 'hb-positions) '((601 0 0 0 0) (279 0 0 0 0) (580 0 0 0 0) (547 0 0 0 0)))))))
