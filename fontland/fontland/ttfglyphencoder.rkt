#lang racket/base
(require "racket.rkt")

(provide TTFGlyphEncoder)

(define-subclass object% (TTFGlyphEncoder)
  (as-methods
   encodeSimple
   _encodePoint))

(define-stub encodeSimple)

(define-stub _encodePoint)