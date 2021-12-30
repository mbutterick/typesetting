#lang racket/base
(provide (all-defined-out))

;; Represents positioning information for a glyph in a GlyphRun.
;; x-advance = The amount to move the virtual pen in the X direction after rendering this glyph.
;; y-advance = The amount to move the virtual pen in the Y direction after rendering this glyph.
;; x-offset = The offset from the pen position in the X direction at which to render this glyph.
;; y-offset = The offset from the pen position in the Y direction at which to render this glyph.
(struct glyph-position (x-advance y-advance x-offset y-offset advance-width) #:transparent #:mutable)

(define (+glyph-position [x-advance 0] [y-advance 0] [x-offset 0] [y-offset 0] [advance-width 0])
  (glyph-position x-advance y-advance x-offset y-offset advance-width))

(define (scale-glyph-position! gp factor)
  (set-glyph-position-x-advance! gp (* factor (glyph-position-x-advance gp)))
  (set-glyph-position-y-advance! gp (* factor (glyph-position-y-advance gp)))
  (set-glyph-position-x-offset! gp (* factor (glyph-position-x-offset gp)))
  (set-glyph-position-y-offset! gp (* factor (glyph-position-y-offset gp)))
  (set-glyph-position-advance-width! gp (* factor (glyph-position-advance-width gp)))
  gp)

(module+ test
  (require rackunit)
  (define gp (+glyph-position 1 2 3 4))
  (check-true (glyph-position? gp))
  (check-equal? (scale-glyph-position! gp 2) (+glyph-position 2 4 6 8 0)))
