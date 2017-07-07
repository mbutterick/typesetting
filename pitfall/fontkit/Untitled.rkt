#lang fontkit/racket
(require "ot-processor.rkt" "glyphinfo.rkt" br/cond)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GSUBProcessor.js
|#

(define-subclass OTProcessor (GSUBProcessor)

  (define/override (applyLookup lookupType table)
    (report/file 'GSUBProcessor:applyLookup)
    (case lookupType
      [(1) ;; Single Substitution
       (report 'single-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define glyph (· this glyphIterator cur))
               (set-field! id glyph
                           (case (· table version)
                             [(1) (bitwise-and (+ (· glyph id) (· table deltaGlyphID)) #xffff)]
                             [(2) (send (· table substitute) get index)]))
               #t])]
      [(2) ;; Multiple Substitution
       (report 'multiple-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define sequence (send (· table sequences) get index))
               (set-field! id (· this glyphIterator cur) (list-ref sequence 0))
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
       (report 'altnernate-substitution)
       (define index (send this coverageIndex (· table coverage)))
       (cond
         [(= index -1) #f]
         [else (define USER_INDEX 0)
               (set-field! id (· this glyphIterator cur) (list-ref (send (· table alternateSet) get index) USER_INDEX))
               #t])]

      [(4) ;; Ligature substitution
       (report 'ligature-substitution)
       (define index (report* (· table coverage) (send this coverageIndex (· table coverage))))
       
       (cond
         [(= index -1) #f]
         [(for* ([ligature (in-list (send (· table ligatureSets) get index))]
                 [matched (in-value (send this sequenceMatchIndices 1 (report* ligature (· ligature components))))]
                 #:when matched)
                (report*/file matched (· this glyphs) index)
                (define curGlyph (· this glyphIterator cur))

                ;; Concatenate all of the characters the new ligature will represent
                (define characters
                  (append (· curGlyph codePoints)
                          (append* (for/list ([index (in-list matched)])
                                             (get-field codePoints (list-ref (· this glyphs) index))))))

                ;; Create the replacement ligature glyph
                (define ligatureGlyph (+GlyphInfo (· this font) (· ligature glyph) characters (· curGlyph features)))
                (set-field! shaperInfo ligatureGlyph (· curGlyph shaperInfo))
                (set-field! ligated ligatureGlyph #t)
                (set-field! substituted ligatureGlyph #t)

                (report 'from-harfbuzz)

                ;; From Harfbuzz:
                ;; - If it *is* a mark ligature, we don't allocate a new ligature id, and leave
                ;;   the ligature to keep its old ligature id.  This will allow it to attach to
                ;;   a base ligature in GPOS.  Eg. if the sequence is: LAM,LAM,SHADDA,FATHA,HEH,
                ;;   and LAM,LAM,HEH for a ligature, they will leave SHADDA and FATHA with a
                ;;   ligature id and component value of 2.  Then if SHADDA,FATHA form a ligature
                ;;   later, we don't want them to lose their ligature id/component, otherwise
                ;;   GPOS will fail to correctly position the mark ligature on top of the
                ;;   LAM,LAM,HEH ligature. See https://bugzilla.gnome.org/show_bug.cgi?id=676343
                ;;
                ;; - If a ligature is formed of components that some of which are also ligatures
                ;;   themselves, and those ligature components had marks attached to *their*
                ;;   components, we have to attach the marks to the new ligature component
                ;;   positions!  Now *that*'s tricky!  And these marks may be following the
                ;;   last component of the whole sequence, so we should loop forward looking
                ;;   for them and update them.
                ;;
                ;;   Eg. the sequence is LAM,LAM,SHADDA,FATHA,HEH, and the font first forms a
                ;;   'calt' ligature of LAM,HEH, leaving the SHADDA and FATHA with a ligature
                ;;   id and component == 1.  Now, during 'liga', the LAM and the LAM-HEH ligature
                ;;   form a LAM-LAM-HEH ligature.  We need to reassign the SHADDA and FATHA to
                ;;   the new ligature with a component value of 2.
                ;;
                ;;   This in fact happened to a font...  See
                ;;   https://bugzilla.gnome.org/show_bug.cgi?id=437633

                
                (define isMarkLigature
                  (and (· curGlyph isMark)
                       (for/and ([match-idx (in-list matched)])
                                (· (list-ref (· this glyphs) match-idx) isMark))))

                (report isMarkLigature)

                (set-field! ligatureID ligatureGlyph (cond
                                                       [isMarkLigature #f]
                                                       [else (define id (· this ligatureID))
                                                             (increment-field! ligatureID this)
                                                             id]))

                (define lastLigID (· curGlyph ligatureID))
                (define lastNumComps (length (· curGlyph codePoints)))
                (define curComps lastNumComps)
                (define idx (add1 (· this glyphIterator index)))

                (report/file 'set-ligature-id)
                ;; Set ligatureID and ligatureComponent on glyphs that were skipped in the matched sequence.
                ;; This allows GPOS to attach marks to the correct ligature components.
                (for ([matchIndex (in-list matched)])
                     (report/file matchIndex)
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
                (set-field! glyphs this (drop-right (· this glyphs) (length matched)))
                (set-field! glyphs this (list-set (· this glyphs) (· this glyphIterator index) ligatureGlyph))
                #t)]
         [else #f])])))

