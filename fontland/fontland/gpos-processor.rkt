#lang racket/base
(require "racket.rkt")

(require "ot-processor.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GPOSProcessor.js
|#

(define-subclass OTProcessor (GPOSProcessor)

  (define/public (applyPositionValue sequenceIndex value)
    (define position (list-ref (· this positions) (send (· this glyphIterator) peekIndex sequenceIndex)))
    (when (· value xAdvance)
      (increment-field! xAdvance position (or (· value xAdvance) 0)))
    (when (· value yAdvance)
      (increment-field! yAdvance position (· value yAdvance)))
    (when (· value xPlacement)
      (increment-field! xOffset position (· value xPlacement)))
    (when (· value yPlacement)
      (increment-field! yOffset position (· value yPlacement))))

  (define/override (applyLookup lookupType table)
    (case lookupType
      [(1) ;; Single positioning value
       #;(report/file 'single-positioning-value)
       (define index (send this coverageIndex (· table coverage)))
       #;(report/file index)
       (cond
         [(= index -1) #f]
         [else #;(report (· table version))
               (case (· table version)
                 [(1) (send this applyPositionValue 0 (· table value))]
                 [(2) (send this applyPositionValue 0 (send (· table values) get index))])
               #t])]
      [(2) ;; Pair Adjustment Positioning
       #;(report/file 'applyLookup:pair-adjustment)
       (define nextGlyph (· this glyphIterator peek))
       (cond
         [(not nextGlyph) #f]
         [else
          #;(report 'getting-pair-coverage-for)
          #;(report* (· this glyphIterator cur id) (· this glyphIterator peek id) (· table coverage))
          (define index (send this coverageIndex (· table coverage)))
          #;(report index)
          (cond
            [(= index -1) #f]
            [else
             #;(report (· table version))
             (case (· table version)
               [(1) ;; Adjustments for glyph pairs
                #;(report 'glyph-pair)
                (define set (send (· table pairSets) get index))
                (for/first ([pair (in-list set)]
                            #:when (= (· pair secondGlyph) (· nextGlyph id)))
                  (send this applyPositionValue 0 (· pair value1))
                  (send this applyPositionValue 0 (· pair value2)))]
               [(2) ;; Class pair adjustment
                #;(report/file 'class-pair)
                (define class1 (send this getClassID (· this glyphIterator cur id) (· table classDef1)))
                (define class2 (send this getClassID (· nextGlyph id) (· table classDef2)))
                (cond
                  [(or (= class1 -1) (= class2 -1)) #f]
                  [else (define pair (send (send (· table classRecords) get class1) get class2))
                        (send this applyPositionValue 0 (· pair value1))
                        (send this applyPositionValue 0 (· pair value2))
                        #t])])])])]
      [(3) ;; Cursive Attachment Positioning
       #;(report/file 'cursive-attachment-positioning-unimplemented)
       (void)]
      [(4) ;; Mark to base positioning
       #;(report/file 'mark-to-base-positioning-unimplemented)
       (void)]
      [(5) ;; Mark to ligature positioning
       #;(report/file 'mark-to-ligature-positioning-unimplemented)
       (void)]
      [(6) ;; Mark to mark positioning
       #;(report/file 'mark-to-mark-positioning-unimplemented)
       (void)]
      [(7) ;; Contextual positioning
       #;(report/file 'contextual-positioning-unimplemented)
       (void)]
      [(8) ;; Chaining contextual positioning
       #;(report/file 'chaining-contextual-positioning-unimplemented)
       (void)]
      [(9) ;; Extension positioning
       #;(report/file 'extension-contextual-positioning-unimplemented)
       (void)]
      [else
       (raise-argument-error 'GPOSProcessor:applyLookup "supported GPOS table" lookupType)]))
       

       

  (define/override (applyFeatures userFeatures glyphs advances)
    (super applyFeatures userFeatures glyphs advances)
    #;(report/file 'fixCursiveAttachment-unimplemented)
    #;(for ([i (in-range (length (· this glyphs)))])
        (send this fixCursiveAttachment i))
    #;(report/file 'fixMarkAttachment-unimplemented)
    #;(send this fixMarkAttachment))

 
  )