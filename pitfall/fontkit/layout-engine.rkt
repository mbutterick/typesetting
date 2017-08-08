#lang fontkit/racket
(require (prefix-in Script- "script.rkt") "glyph.rkt" "glyphrun.rkt" "glyph-position.rkt" "ot-layout-engine.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/layout/LayoutEngine.js
|#

(define-subclass object% (LayoutEngine font)
  (field [unicodeLayoutEngine #f]
         [kernProcessor #f]
         [engine
          ;; Choose an advanced layout engine.
          ;; We try the AAT morx table first since more
          ;; scripts are currently supported because
          ;; the shaping logic is built into the font.
          (cond
            [(· this font has-morx-table?) (error 'morx-layout-unimplemented)]
            [(or (· this font has-gsub-table?) (· this font has-gpos-table?))
             #;(report/file 'starting-layout-engine)
             (+OTLayoutEngine (· this font))]
            [else #f])])

  (as-methods
   layout
   substitute
   position
   hideDefaultIgnorables
   isDefaultIgnorable))

(define/contract (layout this str-or-glyphs [features #f]
                         ;; Attempt to detect the script if not provided.
                         [script (if (string? str-or-glyphs)
                                     (Script-forString str-or-glyphs)
                                     (Script-forCodePoints (append-map (λ (g) (· g codePoints)) str-or-glyphs)))]
                         [language #f])
  (((or/c string? (listof Glyph?))) ((option/c list?) (option/c symbol?) (option/c symbol?)) . ->*m . GlyphRun?)
  
  (define glyphs
    ;; Map string to glyphs if needed
    (if (string? str-or-glyphs)
        (send (· this font) glyphsForString str-or-glyphs)
        str-or-glyphs))

  #;(report*/file 'starting-layout-in-layout-engine glyphs)
  (cond
    [(empty? glyphs) (+GlyphRun glyphs empty)] ; Return early if there are no glyphs
    [else
     ;; Setup the advanced layout engine
     (when (and (· this engine) #;(·? engine setup))
       (send (· this engine) setup glyphs features script language))

     ;; Substitute and position the glyphs
     (set! glyphs (send this substitute glyphs features script language))
     #;(report*/file 'end-sub glyphs)
     #;(error 'stop)
     #;(report/file 'ready-position)
     #;(report (for/list ((g (in-list glyphs))) (· g id)) 'shecky)
     (define positions (send this position glyphs features script language))
     #;(report (for/list ((p (in-list positions))) (list (· p xAdvance) (· p xOffset))))
     #;(report/file 'fired-position)

     ;; Let the layout engine clean up any state it might have
     (when (and (· this engine) #;(·? this engine cleanup))
       (· this engine cleanup))
     (+GlyphRun glyphs positions)]))


(define (substitute this glyphs features script language)
  #;((is-a?/c GlyphRun) . ->m . void?)
  ;; Call the advanced layout engine to make substitutions
  (when (and (· this engine) #;(· this engine substitute))
    (set! glyphs (send (· this engine) substitute glyphs features script language)))
  #;(report/file glyphs)
  glyphs)


(define/contract (position this glyphs features script language)
  ((listof Glyph?) (option/c list?) (option/c symbol?) (option/c symbol?) . ->m . (listof GlyphPosition?))

  (define positions (for/list ([glyph (in-list glyphs)])
                      (make-object GlyphPosition (· glyph advanceWidth))))
  
  ;; Call the advanced layout engine. Returns the features applied.
  (define positioned
    (and (· this engine) #;(· this engine position)
         (send (· this engine) position glyphs positions features script language)))

  ;; if there is no GPOS table, use unicode properties to position marks.
  ;; todo: unicode layout
  #;(unless positioned
    (unless (· this unicodeLayoutEngine)
      (set! unicodeLayoutEngine (+UnicodeLayoutEngine (· this font))))
    (send unicodeLayoutEngine positionGlyphs glyphs positions))

  ;; if kerning is not supported by GPOS, do kerning with the TrueType/AAT kern table
  ;; todo: old style kern table
  #;(when (and (or (not positioned) (not (· positioned kern))) (· this font kern))
      (unless kernProcessor
        (set! kernProcessor (+KernProcessor (· this font))))
      (send kernProcessor process glyphs positions))
  
  positions
  )


(define/contract (hideDefaultIgnorables this glyphRun)
  ((is-a?/c GlyphRun) . ->m . void?)
  (define space (send (· this font) glyphForCodePoint #x20))
  (define-values (new-glyphs new-positions)
    (for/lists (ngs nps)
      ([glyph (in-list (· glyphRun glyphs))]
       [pos (in-list (· glyphRun positions))])
      (cond
        [(send this isDefaultIgnorable (car (· glyph codePoints)))
         (define new-pos pos)
         (set-field! xAdvance new-pos 0)
         (set-field! yAdvance new-pos 0)
         (values space new-pos)]
        [else (values glyph pos)])))
  (set-field! glyphs glyphRun new-glyphs)
  (set-field! positions glyphRun new-positions))


(define/contract (isDefaultIgnorable this codepoint)
  (index? . ->m . boolean?)
  #f ; todo: everything
  )
