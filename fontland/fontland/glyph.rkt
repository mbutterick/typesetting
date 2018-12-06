#lang racket/base
(require (for-syntax racket/base)
         xenomorph
         racket/class
         racket/match
         racket/list
         racket/contract
         sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         sugar/unstable/stub
         "unsafe/freetype.rkt"
         "helper.rkt")
(provide (all-defined-out))
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


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/Glyph.js
|#

#;(define-subclass object% (Glyph id codePoints font)
    (field [_font font]
           [isMark (andmap is-mark? codePoints)]
           [isLigature (> (length codePoints) 1)]
           [_metrics #f])

    (as-methods
     advanceWidth
     _getMetrics))

(struct glyph (id codePoints font _font isMark isLigature _metrics) #:transparent #:mutable)

(define (+glyph id codePoints font
                [_font font]
                [isMark (andmap is-mark? codePoints)]
                [isLigature (> (length codePoints) 1)]
                [_metrics #f]
                #:constructor [constructor glyph])
  (constructor id codePoints font _font isMark isLigature _metrics))

#;(define-stub-stop _getPath)
#;(define-stub-stop _getCBox)
#;(define-stub-stop _getBBox)
#;(define-stub-stop _getTableMetrics)

(define (glyph-advance-width g)
  (hash-ref (_getMetrics g) 'advanceWidth))


(define (_getMetrics g)
  (unless (glyph-_metrics g)
    (define face (· (glyph-_font g) ft-face))
    (FT_Load_Glyph face (glyph-id g) FT_LOAD_NO_RECURSE)
    (define glyph (FT_FaceRec-glyph face))
    (define glyph-metrics (FT_GlyphSlotRec-metrics glyph))
    (set-glyph-_metrics! g (mhash))
    (hash-set*! (glyph-_metrics g)
                'advanceWidth (FT_Glyph_Metrics-horiAdvance glyph-metrics)
                'leftBearing (FT_Glyph_Metrics-horiBearingX glyph-metrics)))
  (glyph-_metrics g))


