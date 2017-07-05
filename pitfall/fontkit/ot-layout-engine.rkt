#lang fontkit/racket
(require "gsub-processor.rkt" "gpos-processor.rkt" "glyphinfo.rkt" (prefix-in Shapers- "shapers.rkt") "shaping-plan.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/OTLayoutEngine.js
|#

(define-subclass object% (OTLayoutEngine font)
  (field [glyphInfos #f]
         [shaper #f]
         [plan #f]
         [GSUBProcessor #f]
         [GPOSProcessor #f])

  (report/file 'starting-ot-layout-engine)
  ;; todo: gsub
  #;(when  (· font has-gsub-table?)
      (set-field! GSUBProcessor this (+GSUBProcessor font (· font GSUB))))

  
  (report* 'dingdong!-starting-gpos)
  (when (· font has-gpos-table?)
      (set-field! GPOSProcessor this (+GPOSProcessor font (· font GPOS))))


  (define/public (setup glyphs features script language)
    ;; Map glyphs to GlyphInfo objects so data can be passed between
    ;; GSUB and GPOS without mutating the real (shared) Glyph objects.
    (set! glyphInfos (map (λ (glyph) (+GlyphInfo (· this font) (· glyph id) (· glyph codePoints))) glyphs))

    ;; Choose a shaper based on the script, and setup a shaping plan.
    ;; This determines which features to apply to which glyphs.
    (set! shaper (Shapers-choose script))
    #;(set! plan (+ShapingPlan (· this font) script language))
    (send (· this shaper) plan (· this plan) (· this glyphInfos) features)))