#lang debug racket

(provide (all-defined-out))

(struct ttf-font (port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc)
  #:mutable
  #:property prop:custom-write
  (λ (f p w?) (display
               (format "<~a ~a>"
                       (object-name f)
                       (let-values ([(dir name _) (split-path (ttf-font-src f))])
                         name)) p)))

(struct woff-font ttf-font ())

(define (ft-face this)
  (or (force (ttf-font-ft-face this)) (error 'ft-face-not-available)))

(define (font-directory this)
  (or (force (ttf-font-directory this))  (error 'directory-not-available)))

(define (hb-font this)
  (or (force (ttf-font-hb-font this)) (error 'hb-font-not-available)))

(define (hb-buf this)
  (or (force (ttf-font-hb-buf this)) (error 'hp-buf-not-available)))

(struct glyph (id codepoints font is-mark? is-ligature? metrics) #:mutable
  #:property prop:custom-write
  (λ (g p w?) (display
               (format "<~a ~a ~a>"
                       (object-name g)
                       (glyph-id g)
                       (glyph-font g)) p)))

(struct ttf-glyph glyph ())

; glyphs = list of glyph ids in the subset
; mapping = of glyph ids to indexes in glyphs
(struct subset (font glyphs mapping) #:mutable)

(struct cff-subset subset (cff strings charstrings gsubrs) #:mutable)

(struct cff-glyph glyph (path _usedGsubrs _usedSubrs) #:mutable)

(define (make-cff-glyph . args)
  (apply cff-glyph (append args (list #f (make-hash) (make-hash)))))

(struct index-item (offset length) #:transparent #:mutable)