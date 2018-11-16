#lang fontkit/racket
(require xenomorph br/cond "opentype.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/GPOS.js
|#

(define ValueFormat (+Bitfield uint16be '(xPlacement yPlacement xAdvance yAdvance xPlaDevice yPlaDevice xAdvDevice yAdvDevice)))

(define types
  (mhash
   'xPlacement int16be
   'yPlacement int16be
   'xAdvance int16be
   'yAdvance int16be
   'xPlaDevice (+Pointer uint16be Device (mhash 'type 'global 'relativeTo 'rel))
   'yPlaDevice (+Pointer uint16be Device (mhash 'type 'global 'relativeTo 'rel))
   'xAdvDevice (+Pointer uint16be Device (mhash 'type 'global 'relativeTo 'rel))
   'yAdvDevice (+Pointer uint16be Device (mhash 'type 'global 'relativeTo 'rel))))

(define-subclass object% (ValueRecord [key 'valueFormat])
  (define/public (buildStruct parent)
    ;; set `struct` to the first dict in the chain of ancestors
    ;; with the target key
    (define struct (let loop ([x parent])
                     (cond
                       [(and x (dict? x) (dict-ref x key #f)) x]
                       [(· x parent) => loop]
                       [else #f])))
    (and struct
         (let ()
           (define format (dict-ref struct key))
           (define fields
             (append
              (dictify 'rel (λ _ (dict-ref struct '_startOffset)))
              (for/list ([(key val) (in-dict format)]
                         #:when val)
                        (cons key (dict-ref types key)))))
           (+Struct fields))))

  (define/public (size val ctx)
    (send (buildStruct ctx) size val ctx))

  (define/public (decode port parent)
    (define res (send (buildStruct parent) decode port parent))
    (dict-remove! res 'rel)
    res)

  (define/public (encode . args)
    (error 'GPOS-encode-not-implemented)))

(define PairValueRecord (+Struct
                         (dictify
                          'secondGlyph uint16be
                          'value1 (+ValueRecord 'valueFormat1)
                          'value2 (+ValueRecord 'valueFormat2))))

(define PairSet (+Array PairValueRecord uint16be))

(define Class2Record (+Struct
                      (dictify
                       'value1 (+ValueRecord 'valueFormat1)
                       'value2 (+ValueRecord 'valueFormat2))))

(define Anchor (+VersionedStruct uint16be
                                 (dictify
                                  ;; Design units only
                                  1 (dictify 'xCoordinate int16be
                                             'yCoordinate int16be)

                                  ;; Design units plus contour point
                                  2 (dictify 'xCoordinate int16be
                                             'yCoordinate int16be
                                             'anchorPoint uint16be)
                                  ;; Design units plus Device tables
                                  3 (dictify 'xCoordinate int16be
                                             'yCoordinate int16be
                                             'xDeviceTable (+Pointer uint16be Device)
                                             'yDeviceTable (+Pointer uint16be Device)))))

(define EntryExitRecord (+Struct
                         (dictify 'entryAnchor (+Pointer uint16be Anchor (mhash 'type 'parent))
                                  'exitAnchor (+Pointer uint16be Anchor (mhash 'type 'parent)))))

(define MarkRecord (+Struct
                    (dictify 'class uint16be
                             'markAnchor uint16be)))

(define MarkArray (+Array MarkRecord uint16be))

(define BaseRecord (+Array (+Pointer uint16be Anchor) (λ (t) (ref* t 'parent 'classCount))))

(define BaseArray (+Array BaseRecord uint16be))

(define ComponentRecord (+Array (+Pointer uint16be Anchor) (λ (t) (ref* t 'parent 'parent 'classCount))))
(define LigatureAttach (+Array ComponentRecord uint16be))
(define LigatureArray (+Array (+Pointer uint16be LigatureAttach) uint16be))

(define-subclass VersionedStruct (GPOSLookup-VersionedStruct))
(define GPOSLookup
  (+GPOSLookup-VersionedStruct
   'lookupType
   (dictify
    ;; Single Adjustment
    1 (+VersionedStruct uint16be
                        (dictify
                         ;; Single positioning value
                         1 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'valueFormat ValueFormat
                            'value (+ValueRecord))
                         2 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'valueFormat ValueFormat
                            'valueCount uint16be
                            'values (+LazyArray (+ValueRecord) 'valueCount))))
    ;; Pair Adjustment Positioning
    2 (+VersionedStruct uint16be
                        (dictify
                         ;; Adjustments for glyph pairs
                         1 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'valueFormat1 ValueFormat
                            'valueFormat2 ValueFormat
                            'pairSetCount uint16be
                            'pairSets (+LazyArray (+Pointer uint16be PairSet) 'pairSetCount))

                         ;; Class pair adjustment
                         2 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'valueFormat1 ValueFormat
                            'valueFormat2 ValueFormat
                            'classDef1 (+Pointer uint16be ClassDef)
                            'classDef2 (+Pointer uint16be ClassDef)
                            'class1Count uint16be
                            'class2Count uint16be
                            'classRecords (+LazyArray (+LazyArray Class2Record 'class2Count) 'class1Count))))

    ;; Cursive Attachment Positioning
    3 (dictify
       'format uint16be
       'coverage (+Pointer uint16be Coverage)
       'entryExitCount uint16be
       'entryExitRecords (+Array EntryExitRecord 'entryExitCount))

    ;; MarkToBase Attachment Positioning
    4 (dictify
       'format uint16be
       'markCoverage (+Pointer uint16be Coverage)
       'baseCoverage (+Pointer uint16be Coverage)
       'classCount uint16be
       'markArray (+Pointer uint16be MarkArray)
       'baseArray (+Pointer uint16be BaseArray))

    ;; MarkToLigature Attachment Positioning
    5 (dictify
       'format uint16be
       'markCoverage (+Pointer uint16be Coverage)
       'ligatureCoverage (+Pointer uint16be Coverage)
       'classCount uint16be
       'markArray (+Pointer uint16be MarkArray)
       'ligatureArray (+Pointer uint16be LigatureArray))

    ;; MarkToMark Attachment Positioning
    6 (dictify
       'format uint16be
       'mark1Coverage (+Pointer uint16be Coverage)
       'mark2Coverage (+Pointer uint16be Coverage)
       'classCount uint16be
       'mark1Array (+Pointer uint16be MarkArray)
       'mark2Array (+Pointer uint16be BaseArray))

    7 Context ;; Contextual positioning
    8 ChainingContext ;; Chaining contextual positioning

    ;; Extension positioning
    9 (dictify
       'posFormat uint16be
       'lookupType uint16be ;; cannot also be 9
       'extension (+Pointer uint32be (λ () (error 'circular-reference-unfixed))))
    )))

;; Fix circular reference
(ref*-set! GPOSLookup 'versions 9 'extension 'type GPOSLookup)

(define-subclass VersionedStruct (GPOS-MainVersionedStruct))
(define GPOS (+GPOS-MainVersionedStruct uint32be
                                        (dictify
                                         'header (dictify 'scriptList (+Pointer uint16be ScriptList)
                                                          'featureList (+Pointer uint16be FeatureList)
                                                          'lookupList (+Pointer uint16be (LookupList GPOSLookup)))
                                         #x00010000 (dictify)
                                         #;#x00010001 #;(+Pointer uint32be FeatureVariations))))

(test-module)