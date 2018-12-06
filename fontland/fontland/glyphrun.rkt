#lang racket/base
(require (prefix-in Script- "script.rkt")
         sugar/unstable/class
         sugar/unstable/js
         racket/class)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/layout/GlyphRun.js
|#

;; Represents a run of Glyph and GlyphPosition objects.
;; Returned by the font layout method.
#;(define-subclass object% (GlyphRun
                            glyphs ; An array of Glyph objects in the run
                            positions) ; An array of GlyphPosition objects for each glyph in the run


    (define/public (advanceWidth)
      (for/sum ([pos (in-list positions)])
               (· pos xAdvance))))


(struct glyphrun (glyphs positions) #:transparent)

(define (advanceWidth gr)
  (for/sum ([pos (in-list (glyphrun-positions gr))])
           (· pos xAdvance)))

(define (append-glyphruns . grs)
  (for/fold ([glyphss null]
             [positionss null]
             #:result (make-object glyphrun
                        (apply append (reverse glyphss))
                        (apply append (reverse positionss))))
            ([gr (in-list grs)])
    (values (cons (get-field glyphs gr) glyphss)
            (cons (get-field positions gr) positionss))))
