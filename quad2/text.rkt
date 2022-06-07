#lang debug racket/base
(require "pipeline.rkt"
         "quad.rkt"
         "attr.rkt"
         "glyphrun.rkt"
         fontland)
(provide (all-defined-out))

(define-pass (measure-text-runs qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (for ([q (in-list qs)]
        #:when (eq? (quad-tag q) 'text-run))
    (define font (get-font (quad-ref q :font-path)))
    (define x-advance (glyph-position-x-advance (vector-ref (glyphrun-positions (layout font (car (quad-elems q)))) 0)))
    (define font-size (quad-ref q :font-size))
    (set-quad-size! q ($size (* (/ x-advance (font-units-per-em font) 1.0) font-size) font-size))))