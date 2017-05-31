#lang pitfall/racket
(require "bbox.rkt" "script.rkt")
(provide GlyphRun)

;; Represents a run of Glyph and GlyphPosition objects.
;; Returned by the font layout method.
(define-subclass object% (GlyphRun
                          glyphs ; An array of Glyph objects in the run
                          features-in 
                          script ; The script that was requested for shaping. This was either passed in or detected automatically.
                          language) ; The language requested for shaping, as passed in. If `null`, the default language for the script was used.

  (super-new)
  
  ;; An array of GlyphPosition objects for each glyph in the run
  (field [positions #f])
  
  ;; The directionality of the requested script (either ltr or rtl).
  (field [direction (script-direction script)])
  
  ;; The features requested during shaping. This is a combination of user
  ;; specified features and features chosen by the shaper.
  (field [features (cond
                     [(hash? features-in) features-in]
                     ;; Convert features to an object
                     [(list? features-in)
                      (define f (mhash))
                      (for ([tag (in-list features)])
                        (hash-set! f tag #t))
                      f]
                     [(not features-in) (mhash)]
                     [else (error 'glyphrun:unknown-features-type)])])

  
  

)