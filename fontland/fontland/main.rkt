#lang racket/base

(define-syntax-rule (r+p ID ...)
  (begin (require ID ...) (provide (all-from-out ID ...))))

(r+p "font.rkt"
     "glyph-position.rkt"
     "subset.rkt"
     "bbox.rkt"
     "tables.rkt"
     "glyphrun.rkt"
     "glyph.rkt"
     "table-stream.rkt"
     "subset.rkt"
     "struct.rkt")