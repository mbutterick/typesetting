#lang fontkit/racket
(require "ot-processor.rkt" "glyphinfo.rkt" br/cond)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GSUBProcessor.js
|#

(define-subclass OTProcessor (GSUBProcessor)

  (define/override (applyLookup lookupType table)
    #;(report lookupType 'GSUBProcessor:applyLookup)
    (case lookupType
      [(1) ;; Single Substitution
       #;(report 'single-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define glyph (· this glyphIterator cur))
               (send glyph id
                     (case (· table version)
                       [(1) (bitwise-and (+ (· glyph id) (· table deltaGlyphID)) #xffff)]
                       [(2) (send (· table substitute) get index)]))
               #t])]
      [(2) ;; Multiple Substitution
       #;(report 'multiple-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define sequence (send (· table sequences) get index))
               (send (· this glyphIterator cur)  id (list-ref sequence 0))
               (set-field! ligatureComponent (· this glyphIterator cur) 0)

               (define features (· this glyphIterator cur features))
               (define curGlyph (· this glyphIterator cur))
               (define replacement (for/list ([(gid i) (in-indexed (cdr sequence))])
                                             (define glyph (+GlyphInfo (· this font) gid #f features))
                                             (set-field! shaperInfo glyph (· curGlyph shaperInfo))
                                             (set-field! isLigated glyph (· curGlyph isLigated))
                                             (set-field! ligatureComponent glyph (add1 i))
                                             (set-field! substituted glyph #t)
                                             glyph))

               (set-field! glyphs this (let-values ([(head tail) (split-at (· this glyphs) (add1 (· this glyphIterator index)))])
                                         (append head replacement tail)))
               #t])]

      [(3) ;; Alternate substitution
       #;(report 'alternate-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define USER_INDEX 0)
               (send (· this glyphIterator cur)  id (list-ref (send (· table alternateSet) get index) USER_INDEX))
               #t])]

      [(4) ;; Ligature substitution
       #;(report '---------------------------)
       #;(report 'ligature-substitution)
       #;(report* lookupType (· table coverage glyphs))
       (define index (send this coverageIndex (· table coverage)))
       #;(report index 'forker)
       (cond
         [(= index -1) #f]
         [(for*/or ([ligature (in-list (send (· table ligatureSets) get index))]
                    [matched (in-value (send this sequenceMatchIndices 1 (· ligature components)))]
                    #:when matched)
                   (define curGlyph (· this glyphIterator cur))

                   ;; Concatenate all of the characters the new ligature will represent
                   (define characters
                     (append (· curGlyph codePoints)
                             (append* (for/list ([index (in-list matched)])
                                                index
                                                (get-field codePoints (list-ref (· this glyphs) index))))))

                   characters
                   ;; Create the replacement ligature glyph
                   (define ligatureGlyph (+GlyphInfo (· this font) (· ligature glyph) characters (· curGlyph features)))
                   (· ligatureGlyph id)
                   (set-field! shaperInfo ligatureGlyph (· curGlyph shaperInfo))
                   (set-field! isLigated ligatureGlyph #t)
                   (set-field! substituted ligatureGlyph #t)

                   (define isMarkLigature
                     (and (· curGlyph isMark)
                          (for/and ([match-idx (in-list matched)])
                                   (· (list-ref (· this glyphs) match-idx) isMark))))

                   (set-field! ligatureID ligatureGlyph (cond
                                                          [isMarkLigature #f]
                                                          [else (define id (· this ligatureID))
                                                                (increment-field! ligatureID this)
                                                                id]))

                   (define lastLigID (· curGlyph ligatureID))
                   (define lastNumComps (length (· curGlyph codePoints)))
                   (define curComps lastNumComps)
                   (define idx (add1 (· this glyphIterator index)))

                   ;; Set ligatureID and ligatureComponent on glyphs that were skipped in the matched sequence.
                   ;; This allows GPOS to attach marks to the correct ligature components.
                   (for ([matchIndex (in-list matched)])
                        ;; Don't assign new ligature components for mark ligatures (see above)
                        (cond
                          [isMarkLigature (set! idx matchIndex)]
                          [else (while (< idx matchIndex)
                                       (define ligatureComponent (+ curComps (- lastNumComps) (min (or (get-field ligatureComponent (list-ref (· this glyphs) idx)) 1) lastNumComps)))
                                       (set-field! ligatureID (list-ref (· this glyphs) idx) (· ligatureGlyph ligatureID))
                                       (set-field! ligatureComponent (list-ref (· this glyphs) idx) ligatureComponent)
                                       (increment! idx))])

                        (define lastLigID (· (list-ref (· this glyphs) idx) ligatureID))
                        (define lastNumComps (length (· (list-ref (· this glyphs) idx) codePoints)))
                        (increment! curComps lastNumComps)
                        (increment! idx)) ;; skip base glyph
                    
                   ;; Adjust ligature components for any marks following
                   (when (and lastLigID (not isMarkLigature))
                     (for ([i (in-range idx (length (· this glyphs)))]
                           #:when (= (· (list-ref (· this glyphs) idx) ligatureID) lastLigID))
                          (define ligatureComponent (+ curComps (- lastNumComps) (min (or (get-field ligatureComponent (list-ref (· this glyphs) i)) 1) lastNumComps)))
                          (set-field! ligatureComponent (list-ref (· this glyphs) i) ligatureComponent)))

                   ;; Delete the matched glyphs, and replace the current glyph with the ligature glyph
                   #;(report (for/list ([g (· this glyphs)]) (· g id)) 'step-a)
                   #;(report matched)
                   #;(report (· this glyphIterator index))
                   (set-field! glyphs this (for*/list ([(glyph idx) (in-indexed (· this glyphs))]
                                                       [midx (in-list matched)]
                                                       #:unless (= idx midx))
                                                      (if (= idx (· this glyphIterator index))
                                                          ligatureGlyph
                                                          glyph)))
                   (set-field! glyphs (· this glyphIterator) (· this glyphs)) ; update glyph iterator to keep it in sync <sigh>
                   #;(report (for/list ([g (· this glyphs)]) (· g id)) 'step-c)
                   #;(report (· this glyphIterator index))
                   #t)]
         [else #f])]
      [(5) ;; Contextual Substitution
       (send this applyContext table)]
      [(6) ;; Chaining Contextual Substitution
       (send this applyChainingContext table)]
      [(7) ;; Extension Substitution
       (send this applyLookup (· table lookupType) (· table extension))]
      [else (error 'unimplemented-gsub-lookup)])))

