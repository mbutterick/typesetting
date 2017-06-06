#lang pitfall/racket
(provide TTFGlyphEncoder)

(define-subclass object% (TTFGlyphEncoder)
  (super-new)

  (as-methods
   encodeSimple
   _encodePoint))

(define-stub encodeSimple)

(define-stub _encodePoint)