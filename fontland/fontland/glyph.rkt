#lang racket/base
(require sugar/unstable/dict
         "unsafe/freetype.rkt"
         "table-stream.rkt"
         "struct.rkt"
         "helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/Glyph.js
|#

; Glyph objects represent a glyph in the font. They have various properties for accessing metrics and
; the actual vector path the glyph represents, and methods for rendering the glyph to a graphics context.
;
; You do not create glyph objects directly. They are created by various methods on the font object.
; There are several subclasses of the base Glyph class internally that may be returned depending
; on the font format, but they all inherit from this class.

(define (is-mark? codepoint)
  ;; mark classes = Mn Me Mc
  (regexp-match #px"\\p{Mn}|\\p{Me}|\\p{Mc}" (string (integer->char codepoint))))

(define (+glyph id codepoints font
                [is-mark? (andmap is-mark? codepoints)]
                [is-ligature? (> (length codepoints) 1)]
                [metrics #f]
                #:constructor [constructor glyph])
  (constructor id codepoints font is-mark? is-ligature? metrics))

(define (glyph-advance-width g)
  (hash-ref (get-glyph-metrics g) 'advanceWidth))

(define (get-glyph-metrics g)
  (unless (glyph-metrics g)
    (define face (ft-face (glyph-font g)))
    (FT_Load_Glyph face (glyph-id g) FT_LOAD_NO_RECURSE)
    (define glyph (FT_FaceRec-glyph face))
    (define ft-glyph-metrics (FT_GlyphSlotRec-metrics glyph))
    (set-glyph-metrics! g (mhash))
    (hash-set*! (glyph-metrics g)
                'advanceWidth (FT_Glyph_Metrics-horiAdvance ft-glyph-metrics)
                'leftBearing (FT_Glyph_Metrics-horiBearingX ft-glyph-metrics)))
  (glyph-metrics g))

(define (+ttf-glyph . args)
  (apply +glyph #:constructor ttf-glyph args))

(define (+cff-glyph . args)
  (apply +glyph #:constructor cff-glyph args))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/TTFFont.js
|#

;; Returns a glyph object for the given glyph id.
;; You can pass the array of code points this glyph represents for
;; your use later, and it will be stored in the glyph object.
(define (get-glyph font gid [codepoints null])
  ((if (has-table? font #"CFF_")
       +cff-glyph
       +ttf-glyph) gid codepoints font))