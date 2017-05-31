#lang pitfall/racket
(provide GlyphPosition)

;; Represents positioning information for a glyph in a GlyphRun.
(define-subclass object% (GlyphPosition
                          ;; The amount to move the virtual pen in the X direction after rendering this glyph.
                          [xAdvance 0]
                          ;; The amount to move the virtual pen in the Y direction after rendering this glyph.
                          [yAdvance 0]
                          ;; The offset from the pen position in the X direction at which to render this glyph.

                          [xOffset 0]
                          ;; The offset from the pen position in the Y direction at which to render this glyph.
                          [yOffset 0])
  (super-new)
  )
