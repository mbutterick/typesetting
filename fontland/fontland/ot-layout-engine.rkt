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

  #;(report/file 'starting-ot-layout-engine)
  (when  (· font has-gsub-table?)
    (set-field! GSUBProcessor this (+GSUBProcessor font (or (· font GSUB) (error 'no-gsub-table)))))

  
  (when (· font has-gpos-table?)
    (set-field! GPOSProcessor this (+GPOSProcessor font (or (· font GPOS) (error 'no-gpos-table)))))


  (define/public (setup glyphs features script language)
    ;; Map glyphs to GlyphInfo objects so data can be passed between
    ;; GSUB and GPOS without mutating the real (shared) Glyph objects.
    (set! glyphInfos (map (λ (glyph) (+GlyphInfo (· this font) (· glyph id) (· glyph codePoints))) glyphs))

    ;; Choose a shaper based on the script, and setup a shaping plan.
    ;; This determines which features to apply to which glyphs.
    (set! shaper (Shapers-choose script))
    (set! plan (+ShapingPlan (· this font) script language))
    #;(report/file shaper)
    (send (make-object shaper) plan (· this plan) (· this glyphInfos) features))

  (define/public (substitute glyphs . _)
    (cond
      [(· this GSUBProcessor)
       #;(report/file (· this glyphInfos))
       (define new-glyphinfos
         (send (· this plan) process (· this GSUBProcessor) (· this glyphInfos)))
       (set! glyphInfos new-glyphinfos) ; update OTLayoutEngine state for positioning pass
       #;(report/file new-glyphinfos)
       ;; Map glyph infos back to normal Glyph objects
       #;(report/file (for/list ([glyphInfo (in-list new-glyphinfos)])
                      (send (· this font) getGlyph (· glyphInfo id) (· glyphInfo codePoints))))
       (for/list ([glyphInfo (in-list new-glyphinfos)])
                      (send (· this font) getGlyph (· glyphInfo id) (· glyphInfo codePoints)))]
      [else glyphs]))

  (define/public (position glyphs positions . _)
    #;(report*/file glyphs positions shaper)
    (define static-shaper (make-object shaper))
    (when (eq? (· static-shaper zeroMarkWidths) 'BEFORE_GPOS)
      (zeroMarkAdvances positions))

    (when GPOSProcessor
      #;(report/file GPOSProcessor)
      (send (· this plan) process GPOSProcessor glyphInfos positions))

    (when (eq? (· static-shaper zeroMarkWidths) 'AFTER_GPOS)
      (zeroMarkAdvances positions))

    ;; Reverse the glyphs and positions if the script is right-to-left
    (when (eq? (· this plan direction) 'rtl)
      (set! glyphs (reverse glyphs))
      (set! positions (reverse positions)))

    #;(report/file (and GPOSProcessor (· GPOSProcessor features)))
    (and GPOSProcessor (· GPOSProcessor features)))


  (define/public (zeroMarkAdvances positions)
    (set! positions
          (for/list ([glyphInfo (in-list glyphInfos)]
                     [position (in-list positions)])
            (when (· glyphInfo isMark)
              (dict-set*! position
                          'xAdvance 0
                          'yAdvance 0))
            position)))

  )