#lang debug racket

(provide (all-defined-out))

(struct ttf-font (port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc)
  #:transparent #:mutable)

(define (ft-face this)
  (or (force (ttf-font-ft-face this)) (error 'ft-face-not-available)))

(define (directory this)
  (or (force (ttf-font-directory directory))  (error 'directory-not-available)))

(define (hb-font this)
  (or (force (ttf-font-hb-font this)) (error 'hb-font-not-available)))

(define (hb-buf this)
  (or (force (ttf-font-hb-buf this)) (error 'hp-buf-not-available)))

(struct glyph (id codepoints font is-mark? is-ligature? metrics) #:transparent #:mutable)

(struct ttf-glyph glyph () #:transparent)