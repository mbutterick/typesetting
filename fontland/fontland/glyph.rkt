#lang racket/base
(require "racket.rkt")

(require "freetype-ffi.rkt")
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

(define-subclass object% (Glyph id codePoints font)
  (field [_font font]
         [isMark (andmap is-mark? codePoints)]
         [isLigature (> (length codePoints) 1)]
         [_metrics #f])

  (as-methods
   _getPath
   _getCBox
   _getBBox
   _getTableMetrics
   advanceWidth
   _getMetrics))

(define-stub-stop _getPath)
(define-stub-stop _getCBox)
(define-stub-stop _getBBox)
(define-stub-stop _getTableMetrics)

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

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/CFFGlyph.js
|#

(define-subclass Glyph (CFFGlyph)
  (error 'cff-glyph-unimplemented)

  #;(define/override (_getName this)
    (->m any/c)
    (if (send (· this _font) _getTable 'CFF2)
        (super _getName)
        (send (send (· this _font) _getTable 'CFF_) getGlyphName (· this id))))
  (as-methods
   #;_getName
   #;bias
   #;_getPath))




