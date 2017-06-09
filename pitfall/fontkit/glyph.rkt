#lang fontkit/racket
(require "freetype-ffi.rkt")
(provide Glyph CFFGlyph TTFGlyph)
(module+ test (require rackunit))

#|
/**
 * Glyph objects represent a glyph in the font. They have various properties for accessing metrics and
 * the actual vector path the glyph represents, and methods for rendering the glyph to a graphics context.
 *
 * You do not create glyph objects directly. They are created by various methods on the font object.
 * There are several subclasses of the base Glyph class internally that may be returned depending
 * on the font format, but they all inherit from this class.
 */
|#


;; approximates
;; https://github.com/devongovett/fontkit/blob/master/src/glyph/Glyph.js

(define (is-mark? codepoint)
  ;; mark classes = Mn Me Mc
  (regexp-match #px"\\p{Mn}|\\p{Me}|\\p{Mc}" (string (integer->char codepoint))))

(module+ test
  (check-true (and (is-mark? #x300) #t))
  (check-false (and (is-mark? #x2ee) #t)))

(define-subclass object% (Glyph id codePoints font)
  (field [_font font]
         [isMark (andmap is-mark? codePoints)]
         [isLigature (> (length codePoints) 1)]
         [_metrics #f])

  (as-methods
   advanceWidth
   _getMetrics))


(define/contract (advanceWidth this)
  (->m number?)
  (hash-ref (_getMetrics this) 'advanceWidth))


(define/contract (_getMetrics this)
  (->m hash?)
  (unless (· this _metrics)
    (define face (· this _font ft-face))
    (FT_Load_Glyph face (· this id) FT_LOAD_NO_RECURSE)
    (define glyph (FT_FaceRec-glyph face))
    (define glyph-metrics (FT_GlyphSlotRec-metrics glyph))
    (set-field! _metrics this (mhash))
    (hash-set*! (· this _metrics)
                'advanceWidth (FT_Glyph_Metrics-horiAdvance glyph-metrics)
                'leftBearing (FT_Glyph_Metrics-horiBearingX glyph-metrics)))
  (· this _metrics))


(define-subclass Glyph (CFFGlyph)
  (error 'cff-glyph-unimplemented))


(define-subclass Glyph (TTFGlyph)

  )