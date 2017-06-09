#lang fontkit/racket
(provide TTFGlyphEncoder)

(define-subclass object% (TTFGlyphEncoder)
  (as-methods
   encodeSimple
   _encodePoint))

(define-stub encodeSimple)

(define-stub _encodePoint)