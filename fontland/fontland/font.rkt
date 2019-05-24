#lang debug racket/base
(require "helper.rkt"
         "unsafe/freetype.rkt"
         "glyph.rkt"
         "bbox.rkt"
         "glyphrun.rkt"
         "directory.rkt"
         "woff-directory.rkt"
         "struct.rkt"
         "table-stream.rkt"
         xenomorph
         racket/match
         racket/list
         sugar/unstable/dict
         "unsafe/harfbuzz.rkt"
         "glyph-position.rkt"
         sugar/list
         racket/promise)
(provide (all-defined-out))



(define ft-library (delay (FT_Init_FreeType)))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/TTFFont.js
|#

(struct probe-fail exn ())

(define (+ttf-font port
                   #:directory [directory-class directory]
                   #:probe [probe-vals (list #"true" #"OTTO" (bytes 0 1 0 0))]
                   #:constructor [structor ttf-font])
  
  (unless (input-port? port)
    (raise-argument-error '+ttf-font "input port" port))
  (unless (member (peek-bytes 4 0 port) probe-vals)
    (raise (probe-fail "fail" (current-continuation-marks))))
  
  (define decoded-tables (mhash))
  (define src (path->string (object-name port)))
  (define directory (delay (decode directory-class port #:parent (mhash x:start-offset-key 0))))
  (define ft-face (delay (and src (FT_New_Face (force ft-library) src))))
  (define hb-font (delay (and src (hb_ft_font_create (force ft-face)))))
  (define hb-buf (delay (hb_buffer_create)))
  (define crc (equal-hash-code port))
  (define get-head-table-proc #f)
 
  (define font
    (structor port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc))
  ;; needed for `loca` table decoding cross-reference
  (set-ttf-font-get-head-table-proc! font (delay (get-head-table font)))
  font)

(define (font-postscript-name font) (FT_Get_Postscript_Name (ft-face font)))
(define (font-units-per-em font) (hash-ref (get-head-table font) 'unitsPerEm))
(define (font-ascent font)  (hash-ref (get-hhea-table font) 'ascent))
(define (font-descent font) (hash-ref (get-hhea-table font) 'descent))
(define (font-linegap font) (hash-ref (get-hhea-table font) 'lineGap))
(define (font-underline-position font) (hash-ref (get-post-table font) 'underlinePosition))
(define (font-underline-thickness font) (hash-ref (get-post-table font) 'underlineThickness))
(define (font-italic-angle font) (hash-ref (get-post-table font) 'italicAngle))

(define (font-cap-height font)
  (if (has-table? font #"OS/2")
      (hash-ref (get-OS/2-table font) 'capHeight)
      (font-ascent font)))

(define (font-x-height font)
  (if (has-table? font #"OS/2")
      (hash-ref (get-OS/2-table font) 'xHeight)
      0))

(define (font-bbox font)
  (define head-table (get-head-table font))
  (+bbox (hash-ref head-table 'xMin) (hash-ref head-table 'yMin)
         (hash-ref head-table 'xMax) (hash-ref head-table 'yMax)))


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/WOFFFont.js
|#

(define (+woff-font port)
  (+ttf-font port
             #:directory woff-directory
             #:probe (list #"wOFF")
             #:constructor woff-font))

;; 181228: disk-based caching (either with sqlite or `with-cache`) is a loser
;; reads & writes aren't worth it vs. recomputing
;; (though this is good news, as it avoids massive disk caches hanging around)
;; ram cache in pitfall suffices

(define (layout font str
                #:features [features null]
                #:script [script #f]
                #:language [lang #f]
                #:direction [direction #f])
  (match (for/list ([c (in-string str)]) (char->integer c))
    [(? null?) (glyphrun (vector) (vector))]
    [codepoints
     (define buf (hb-buf font))
     (hb_buffer_reset buf)
     (when script
       (hb_buffer_set_script buf script))
     (when lang
       (hb_buffer_set_language buf (hb_language_from_string lang)))
     (when direction
       (hb_buffer_set_direction buf direction))
     (hb_buffer_add_codepoints buf codepoints)
     (hb_shape (hb-font font) buf (map (λ (fpr) (tag->hb-feature (car fpr) (cdr fpr))) features))
     (define gis (hb_buffer_get_glyph_infos buf))
     (define hb-gids (map hb_glyph_info_t-codepoint gis))
     ;; `remove-duplicates` in case we get a funny codepoint that expands into multiple glyphs (rare but possible)
     (define hb-clusters (break-at codepoints (remove-duplicates (map hb_glyph_info_t-cluster gis) eq?)))
     (define hb-positions (map hb_glyph_position_t->list (hb_buffer_get_glyph_positions buf)))
     (define glyphs (for/vector ([gidx (in-list hb-gids)]
                                 [cluster (in-list hb-clusters)])
                      (get-glyph font gidx cluster)))
     (define positions (for/vector ([posn (in-list hb-positions)])
                         (apply +glyph-position posn)))
     (glyphrun glyphs positions)]))


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/index.js
|#

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/base.js
|#

(define (open-font str-or-path)
  (define filename (if (path? str-or-path) (path->string str-or-path) str-or-path))
  (define port (open-input-file filename))
  ;; rather than use a `probe` function,
  ;; just try making a font with each format and see what happens
  (or
   (for/or ([font-constructor (in-list (list +ttf-font +woff-font))])
     (with-handlers ([probe-fail? (λ (exn) #f)])
       (font-constructor port)))
   (error 'create-font "unknown font format")))

(module+ test
  (require rackunit racket/struct racket/vector)
  (define charter (open-font charter-path))
  (define charter-woff (open-font charter-woff-path))
  (define fira (open-font (path->string fira-path)))
  (define otf (open-font (path->string fira-otf-path)))
  (for ([charter (list charter charter-woff)])
    (check-equal? (font-postscript-name charter) "Charter")
    (check-equal? (font-units-per-em charter) 1000)
    (check-equal? (font-ascent charter) 980)
    (check-equal? (font-descent charter) -238)
    (check-equal? (font-linegap charter) 0)
    (check-equal? (font-underline-thickness charter) 58)
    (check-equal? (font-italic-angle charter) 0)
    (check-equal? (font-cap-height charter) 671)
    (check-equal? (font-x-height charter) 481)
    (check-equal? (bbox->list (font-bbox charter)) '(-161 -236 1193 963))
    (check-equal? (glyph-position-x-advance (vector-ref (glyphrun-positions (layout charter "f")) 0)) 321)
    (check-true (has-table? charter #"cmap"))
    (check-exn exn:fail:contract? (λ () (get-table charter 'nonexistent-table-tag))))
  (check-true
   (let ([gr (layout fira "Rifle")])
     (and (equal? (vector-map glyph-id (glyphrun-glyphs gr)) '#(227 480 732 412))
          (equal? (vector-map struct->list (glyphrun-positions gr)) '#((601 0 0 0 0) (279 0 0 0 0) (580 0 0 0 0) (547 0 0 0 0)))))))
