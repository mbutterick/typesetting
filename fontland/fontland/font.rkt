#lang debug racket/base
(require "helper.rkt"
         "unsafe/freetype.rkt"
         "glyph.rkt"
         "bbox.rkt"
         "glyphrun.rkt"
         "directory.rkt"
         "db.rkt"
         "struct.rkt"
         "table-stream.rkt"
         xenomorph
         racket/match
         sugar/unstable/dict
         sugar/unstable/js
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
 (define f (open-font charter-path))
 (define fira (open-font (path->string fira-path)))
 (define otf (open-font (path->string fira-otf-path)))
 (check-equal? (font-postscript-name f) "Charter"))

(define ft-library (delay (FT_Init_FreeType)))

(define (+ttf-font port
                   [decoded-tables (mhash)]
                   [src (path->string (object-name port))]
                   [directory (delay (parameterize ([current-parent (mhash '_startOffset 0)])
                                       (xdecode Directory port)))]
                   [ft-face (delay (and src (FT_New_Face (force ft-library) src)))]
                   [hb-font (delay (and src (hb_ft_font_create (force ft-face))))]
                   [hb-buf (delay (hb_buffer_create))]
                   [crc (begin0 (crc32c-input-port port) (pos port 0))]
                   [get-head-table-proc #f])
  (unless (input-port? port)
    (raise-argument-error '+ttf-font "input port" port))
  (unless (member (peek-bytes 4 0 port) (list #"true" #"OTTO" (bytes 0 1 0 0)))
    (do-probe-fail!))
  (define font
    (ttf-font port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc))
  ;; needed for `loca` table decoding cross-reference
  (set-ttf-font-get-head-table-proc! font (delay  (get-head-table font)))
  font)

(define (font-postscript-name font) (FT_Get_Postscript_Name (ft-face font)))
(define (font-units-per-em font) (· (get-head-table font) unitsPerEm))
(define (font-ascent font)  (· (get-hhea-table font) ascent))
(define (font-descent font) (· (get-hhea-table font) descent))
(define (font-linegap font) (· (get-hhea-table font) lineGap))
(define (font-underline-position font) (· (get-post-table font) underlinePosition))
(define (font-underline-thickness font) (· (get-post-table font) underlineThickness))
(define (font-italic-angle font) (· (get-post-table font) italicAngle))

(define (font-cap-height font)
  (if (has-table? font #"OS/2")
      (· (get-OS/2-table font) capHeight)
      (font-ascent font)))

(define (font-x-height font)
  (if (has-table? font #"OS/2")
      (· (get-OS/2-table font) xHeight)
      0))

(test-module
 (check-equal? (font-units-per-em f) 1000)
 (check-equal? (font-ascent f) 980)
 (check-equal? (font-descent f) -238)
 (check-equal? (font-linegap f) 0)
 (check-equal? (font-underline-position f) -178)
 (check-equal? (font-underline-thickness f) 58)
 (check-equal? (font-italic-angle f) 0)
 (check-equal? (font-cap-height f) 671)
 (check-equal? (font-x-height f) 481))

(define (font-bbox font)
  (define head-table (get-head-table font))
  (+bbox (· head-table xMin) (· head-table yMin) (· head-table xMax) (· head-table yMax)))

(test-module
 (check-equal? (bbox->list (font-bbox f)) '(-161 -236 1193 963)))

(define current-layout-caching (make-parameter #false))

(struct hb-gid (val) #:transparent)
(struct hb-cluster (chars) #:transparent)
(struct hb-position (xad yad xoff yoff etc) #:transparent)
(struct hb-layout (hb-gids hb-clusters hb-positions) #:transparent)

(define hb-output (+xstruct 'hb-gids (+xarray #:type uint16 #:length uint16)
                            'hb-clusters (+xarray #:type (+xarray #:type uint16 #:length uint16) #:length uint16)
                            'hb-positions (+xarray #:type (+xarray #:type uint16 #:length 5) #:length uint16)))

(define (hb-layout->glyphrun font hbr)
  (match hbr
    [(hash-table ('hb-gids gids)
                 ('hb-clusters clusters)
                 ('hb-positions posns))
     (define glyphs (for/list ([gidx (in-list gids)]
                               [cluster (in-list clusters)])
                              (get-glyph font gidx cluster)))
     (define positions (for/list ([posn (in-list posns)])
                                 (apply +glyph-position posn)))
     (glyphrun glyphs positions)]))

(define (harfbuzz-layout font codepoints features script language)
  (define buf (hb-buf font))
  (hb_buffer_reset buf)
  (hb_buffer_add_codepoints buf codepoints)
  (define chars (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf)))
  (hb_shape (hb-font font) buf (map tag->hb-feature (or features null)))
  (define gis (hb_buffer_get_glyph_infos buf))
  (dictify 'hb-gids (map hb_glyph_info_t-codepoint gis)
           'hb-clusters (break-at chars (map hb_glyph_info_t-cluster gis))
           'hb-positions (map hb_glyph_position_t->list (hb_buffer_get_glyph_positions buf))))

(define layout-cache (make-hasheqv))

(define hb-input (+xstruct 'font-crc uint32
                           'codepoints (+xarray #:type uint16)
                           'userFeatures (+xarray #:type (+xstring #:length uint8))))

(define (layout-cache-key font-crc codepoints user-features . _)
  (crc32c-bytes (encode hb-input (dictify 
                                  'font-crc font-crc
                                  'codepoints codepoints
                                  'userFeatures user-features) #f)))


;; Returns a GlyphRun object, which includes an array of Glyphs and GlyphPositions for the given string.
(define (layout font string [user-features #f] [script #f] [language #f] #:test [test #f])
  #;((string?) ((option/c (listof symbol?)) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  (define (get-layout string)
    (define codepoints (map char->integer (string->list string)))
    (define args (list codepoints (if user-features (sort user-features symbol<?) null) script language))
    (define key (apply layout-cache-key (ttf-font-crc font) args))
    (hash-ref! layout-cache key
               (λ ()
                 #;(encode hb-output (apply harfbuzz-layout font args) #f)
                 (match (get-layout-from-db key)
                   [(? bytes? res) (decode hb-output res)]
                   [_  (define new-layout (apply harfbuzz-layout font args))
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
     (apply append-glyphruns (map (λ (layout) (hb-layout->glyphrun font layout)) (map get-layout substrs)))]
    [else (if test
              (get-layout string)
              (hb-layout->glyphrun font (get-layout string)))]))


;; Returns an array of Glyph objects for the given string.
;; This is only a one-to-one mapping from characters to glyphs.
;; For most uses, you should use font.layout (described below), which
;; provides a much more advanced mapping supporting AAT and OpenType shaping.
(define (glyphs-for-string font string)
  #;(string? . ->m . (listof glyph?))
  ;; todo: make this handle UTF-16 with surrogate bytes
  ;; for now, just use UTF-8
  (define codepoints (map char->integer (string->list string)))
  (for/list ([cp (in-list codepoints)])
            (glyph-for-codepoint font cp)))

;; Maps a single unicode code point to a Glyph object.
;; Does not perform any advanced substitutions (there is no context to do so).
(define (glyph-for-codepoint font codepoint)
  (define glyph-idx (FT_Get_Char_Index (· font ft-face) codepoint))
  (get-glyph font glyph-idx (list codepoint)))

(define (measure-char-width font char)
  (define glyph-idx (FT_Get_Char_Index (ft-face font) (char->integer char)))
  (FT_Load_Glyph (ft-face font) glyph-idx FT_LOAD_NO_RECURSE)
  (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph (ft-face font)))))
  (* width 1.0))

(define (measure-string font str size)
  (/ (* size
        (for/sum ([c (in-string str)])
                 (measure-char-width font c))) (font-units-per-em font)))

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

(define (open-font str-or-path)
  (define filename (if (path? str-or-path) (path->string str-or-path) str-or-path))
  (create-font (open-input-file filename)))

(struct probe-fail exn ())
(define (do-probe-fail!)
  (raise (probe-fail "fail" (current-continuation-marks))))

(define (create-font port)
  (or
   ;; rather than use a `probe` function,
   ;; just try making a font with each format and see what happens
   (for/first ([font-format (in-list font-formats)])
              (with-handlers ([probe-fail? (λ (exn) #f)])
                (font-format port)))
   (error 'create-font "unknown font format")))

(module+ test
  (require rackunit racket/dict)
  (check-equal? (measure-string f "f" (font-units-per-em f)) 321.0)
  (check-true (has-table? f #"cmap"))
  (check-exn exn:fail:contract? (λ () (get-table f 'nonexistent-table-tag)))
  (check-true
   (let ([h (layout fira "Rifle" #:test #t)])
     (and (equal? (dict-ref h 'hb-gids) '(227 480 732 412))
          (equal? (dict-ref h 'hb-clusters) '((82) (105) (102 108) (101)))
          (equal? (dict-ref h 'hb-positions) '((601 0 0 0 0) (279 0 0 0 0) (580 0 0 0 0) (547 0 0 0 0)))))))
