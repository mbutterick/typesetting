#lang fontkit/racket
(provide (all-defined-out))

;; Represents positioning information for a glyph in a GlyphRun.
(define-subclass object% (GlyphPosition
                          ;; The amount to move the virtual pen in the X direction after rendering this glyph.
                          [xAdvance 0]
                          ;; The amount to move the virtual pen in the Y direction after rendering this glyph.
                          [yAdvance 0]
                          ;; The offset from the pen position in the X direction at which to render this glyph.

                          [xOffset 0]
                          ;; The offset from the pen position in the Y direction at which to render this glyph.
                          [yOffset 0]
                          [advanceWidth 0])

  (as-methods
   scale)
  )


(define/contract (scale this factor)
  (number? . ->m . (is-a?/c GlyphPosition))
  (set-field! xAdvance this (* factor (· this xAdvance)))
  (set-field! yAdvance this (* factor (· this yAdvance)))
  (set-field! xOffset this (* factor (· this xOffset)))
  (set-field! yOffset this (* factor (· this yOffset)))
  (set-field! advanceWidth this (* factor (· this advanceWidth)))
  this)
