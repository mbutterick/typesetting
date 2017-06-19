#lang fontkit/racket
(require "script.rkt" "glyph.rkt" "glyphrun.rkt" "glyph-position.rkt")
(provide LayoutEngine)

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
             (displayln 'warning:ot-layout-unimplemented) #f]
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
                                     (script-for-string str-or-glyphs)
                                     (script-for-codepoints (append-map (λ (g) (· g codePoints)) str-or-glyphs)))]
                         [language #f])
  (((or/c string? (listof (is-a?/c Glyph)))) ((or/c list? #f) (or/c symbol? #f) (or/c symbol? #f)) . ->*m . (is-a?/c GlyphRun))
  
  (define glyphs
    (if (string? str-or-glyphs)
        (send (· this font) glyphsForString str-or-glyphs)
        str-or-glyphs))

  (define glyphRun (make-object GlyphRun glyphs features script language))

  (if (empty? glyphs)
      (set-field! positions glyphRun empty)
      (begin
        ;; Setup the advanced layout engine ; todo

        ;; Substitute and position the glyphs
        (send this substitute glyphRun)
        (send this position glyphRun)
        (send this hideDefaultIgnorables glyphRun)

        ;; Let the layout engine clean up any state it might have
        (and (· this engine) (· this engine cleanup))))
  
  glyphRun)


(define/contract (substitute this glyphRun)
  ((is-a?/c GlyphRun) . ->m . void?)
  ;; Call the advanced layout engine to make substitutions
  (when (and (· this engine) (· this engine substitute))
    (send (· this engine) substitute glyphRun)))


(define/contract (position this glyphRun)
  ((is-a?/c GlyphRun) . ->m . void?)

  (define positions (for/list ([g (in-list (· glyphRun glyphs))])
                      (make-object GlyphPosition (· g advanceWidth))))
  (set-field! positions glyphRun positions)
  
  ;; Call the advanced layout engine. Returns the features applied.
  (define positioned
    (and (· this engine) (· this engine position)
         (send (· this engine) position glyphRun)))

  ;; if there is no GPOS table, use unicode properties to position marks.
  ;; todo: implement unicodelayoutengine


  ;; if kerning is not supported by GPOS, do kerning with the TrueType/AAT kern table
  ;; todo: implement kerning
  (void)
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
