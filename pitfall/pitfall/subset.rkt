#lang pitfall/racket
(provide Subset CFFSubset TTFSubset)

;; approximates
;; https://github.com/devongovett/fontkit/blob/master/src/subset/Subset.js

(define-subclass object% (Subset font)
  (super-new)
  (field [glyphs empty] ; list of glyphs in the subset
         [mapping (mhash)] ; mapping of glyph ids to indexes in `glyphs`
         )

  (send this includeGlyph 0) ; always include the missing glyph in subset

  (as-methods
   includeGlyph))

(define/contract (includeGlyph this glyph)
  ((or/c object? index?) . ->m . index?)
  (let ([glyph (if (object? glyph) (· glyph id) glyph)])
    (hash-ref! (· this mapping) glyph
               (λ ()
                 ;; put the new glyph at the end of `glyphs`,
                 ;; and put its index in the mapping
                 (push-end-field! glyphs this glyph)
                 (sub1 (length (· this glyphs)))))))  


(define-subclass Subset (CFFSubset)
  (super-new)
  (error 'cff-subset-unimplemented))


;; approximates
;; https://github.com/devongovett/fontkit/blob/master/src/subset/TTFSubset.js

(define-subclass Subset (TTFSubset)
  (super-new)
  )