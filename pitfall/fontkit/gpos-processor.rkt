#lang fontkit/racket
(require "ot-processor.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GPOSProcessor.js
|#

(define-subclass OTProcessor (GPOSProcessor)

  (define/override (applyLookup lookupType table)
    (report/file 'starting-applyLookup)
    (case lookupType
      [(1) ;; Single positioning value
       (report/file 'single-positioning-value)
       (define index (send this coverageIndex (· table coverage)))
       (report/file index)
       (cond
         [(= index -1) #f]
         [else (case (· table version)
                 [(1) (send this applyPositionValue 0 (· table value))]
                 [(2) (send this applyPositionValue 0 (send (· table values) get index))])
               #t])]
      [(2) ;; Pair Adjustment Positioning
       (report/file 'pair-adjustment)
       (define nextGlyph (· this glyphIterator peek))
       (report/file nextGlyph)
       (cond
         [(not nextGlyph) #f]
         [else
          (define index (send this coverageIndex (· table coverage)))
          (report/file index)
          (cond
            [(= index -1) #f]
            [else
             (case (· table version)
               [(1) ;; Adjustments for glyph pairs
                (report/file 'glyph-pair)
                (define set (send (· table pairSets) get index))
                (for/first ([pair (in-list set)]
                            #:when (= (· pair secondGlyph) (· nextGlyph id)))
                  (send this applyPositionValue 0 (· pair value1))
                  (send this applyPositionValue 0 (· pair value2)))]
               [(2) ;; Class pair adjustment
                (report/file 'class-pair)
                (define class1 (send this getClassId (· this glyphIterator cur id) (· table classDef1)))
                (define class2 (send this getClassId (· nextGlyph id) (· table classDef2)))
                (cond
                  [(or (= class1 -1) (= class2 -1)) #f]
                  [else (define pair (send (send (· table classRecords) get class1) get class2))
                        (send this applyPositionValue 0 (· pair value1))
                        (send this applyPositionValue 0 (· pair value2))
                        #t])])])])]
      [(3) ;; Cursive Attachment Positioning
       (report/file 'cursive-attachment-positioning-unimplemented)
       (error)]
      [(4) ;; Mark to base positioning
       (report/file 'mark-to-base-positioning-unimplemented)
       (error)]
      [(5) ;; Mark to ligature positioning
       (report/file 'mark-to-ligature-positioning-unimplemented)
       (error)]
      [(6) ;; Mark to mark positioning
       (report/file 'mark-to-mark-positioning-unimplemented)
       (error)]
      [(7) ;; Contextual positioning
       (report/file 'contextual-positioning-unimplemented)
       (error)]
      [(8) ;; Chaining contextual positioning
       (report/file 'chaining-contextual-positioning-unimplemented)
       (error)]
      [(9) ;; Extension positioning
       (report/file 'extension-contextual-positioning-unimplemented)
       (error)]
      [else
       (raise-argument-error 'GPOSProcessor:applyLookup "supported GPOS table" lookupType)]))
       

       

  (define/override (applyFeatures userFeatures glyphs advances)
    (super applyFeatures userFeatures glyphs advances)
    (for ([i (in-range (length (· this glyphs)))])
      (send this fixCursiveAttachment i))
    (send this fixMarkAttachment))

 
  )