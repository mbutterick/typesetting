#lang racket/base
(require sugar/unstable/js
         racket/class
         "glyph-position.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/layout/GlyphRun.js
|#

;; Represents a run of Glyph and GlyphPosition objects.
;; Returned by the font layout method.
; An array of Glyph objects in the run
; An array of GlyphPosition objects for each glyph in the run
(struct glyphrun (glyphs positions) #:transparent)

(define (+glyphrun [glyphs null] [positions null])
  (glyphrun glyphs positions))

(define (glyphrun-advance-width gr)
  (for/sum ([pos (in-list (glyphrun-positions gr))])
           (glyph-position-x-advance pos)))

(define (append-glyphruns . grs)
  (for/fold ([glyphss null]
             [positionss null]
             #:result (glyphrun
                        (apply append (reverse glyphss))
                        (apply append (reverse positionss))))
            ([gr (in-list grs)])
    (values (cons (glyphrun-glyphs gr) glyphss)
            (cons (glyphrun-positions gr) positionss))))

(module+ test
  (require rackunit)
  (define gr (+glyphrun))
  (check-true (glyphrun? gr))
  (check-equal? (append-glyphruns gr gr) gr))
