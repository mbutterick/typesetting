#lang debug racket

(provide (all-defined-out))

(struct ttf-font (port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc)
  #:transparent #:mutable)

(define (ft-face this)
  (or (force (ttf-font-ft-face this)) (error 'ft-face-not-available)))

(define (font-directory this)
  (or (force (ttf-font-directory this))  (error 'directory-not-available)))

(define (hb-font this)
  (or (force (ttf-font-hb-font this)) (error 'hb-font-not-available)))

(define (hb-buf this)
  (or (force (ttf-font-hb-buf this)) (error 'hp-buf-not-available)))

(struct glyph (id codepoints font is-mark? is-ligature? metrics) #:transparent #:mutable)

(struct ttf-glyph glyph () #:transparent)

; glyphs = list of glyph ids in the subset
; mapping = of glyph ids to indexes in glyphs
(struct subset (font glyphs mapping) #:transparent #:mutable)

(struct cff-subset subset (cff strings charstrings gsubrs) #:transparent #:mutable)

(struct cff-glyph glyph (path _usedGsubrs _usedSubrs)
  #:transparent #:mutable)

(define (make-cff-glyph . args)
  (apply cff-glyph (append args (list #f (make-hash) (make-hash)))))