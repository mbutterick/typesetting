#lang fontkit/racket
(require restructure br/cond "opentype.rkt")
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
   'xPlaDevice uint16be ;; pointer
   'yPlaDevice uint16be ;; pointer
   'xAdvDevice uint16be ;; pointer
   'yAdvDevice uint16be)) ;; pointer

(define-subclass RestructureBase (ValueRecord [key 'valueFormat])

  (define/public (buildStruct parent)
    (define struct parent)
    (while (and (not (hash-ref struct (· this key))) (hash-ref struct parent))
           (hash-set! struct (hash-ref struct parent)))

    (cond
      [(not (hash-ref struct (· this key))) (void)]
      [else (define fields (mhash))
            (hash-set! fields 'rel (λ () (hash-ref struct (error '_startOffset-not-available))))

            (define format (hash-ref struct (· this key)))
            (for ([key (in-list format)])
                 (when (hash-ref format key)
                   (hash-set! fields key (hash-ref types key))))
            (+Struct fields)]))

  (define/override (size val ctx)
    (send (buildStruct ctx) size val ctx))

  
  (define/override (decode stream parent)
    (define res (send (buildStruct parent) decode stream parent))
    (hash-remove! res 'rel)
    res)

  (define/override (encode . args)
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
                                             'xDeviceTable uint16be  ; pointer
                                             'yDeviceTable uint16be)))) ; pointer

(define EntryExitRecord (+Struct
                         (dictify 'entryAnchor uint16be ; pointer
                                  'exitAnchor uint16be))) ; pointer

(define MarkRecord (+Struct
                    (dictify 'class uint16be
                             'markAnchor uint16be)))

(define MarkArray (+Array MarkRecord uint16be))

(define BaseRecord (+Array uint16be)) ; pointer

(define BaseArray (+Array BaseRecord uint16be))

(define ComponentRecord (+Array uint16be)) ; pointer
(define LigatureAttach (+Array ComponentRecord uint16be))
(define LigatureArray (+Array uint16be uint16be)) ; pointer

(define-subclass VersionedStruct (GPOSLookup-VersionedStruct))
(define GPOSLookup
  (+GPOSLookup-VersionedStruct
   (λ (parent) (· parent lookupType))
   (dictify
    ;; Single Adjustment
    1 (+VersionedStruct uint16be
                        (dictify
                         ;; Single positioning value
                         1 (dictify
                            'coverage uint16be ; pointer
                            'valueFormat ValueFormat
                            'value (+ValueRecord))
                         2 (dictify
                            'coverage uint16be ; pointer
                            'valueFormat ValueFormat
                            'valueCount uint16be
                            'values (+LazyArray (+ValueRecord) 'valueCount))))
    ;; Pair Adjustment Positioning
    2 (+VersionedStruct uint16be
                        (dictify
                         ;; Adjustments for glyph pairs
                         1 (dictify
                            'coverage uint16be ; pointer
                            'valueFormat1 ValueFormat
                            'valueFormat2 ValueFormat
                            'pairSetCount uint16be
                            'pairSets (+LazyArray uint16be 'pairSetCount)) ; pointer

                         ;; Class pair adjustment
                         2 (dictify
                            'coverage uint16be ; pointer
                            'valueFormat1 ValueFormat
                            'valueFormat2 ValueFormat
                            'classDef1 uint16be ; pointer
                            'classDef2 uint16be ; pointer
                            'class1Count uint16be
                            'class2Count uint16be
                            'classRecords (+LazyArray (+LazyArray Class2Record 'class2Count) 'class1Count))))

    ;; Cursive Attachment Positioning
    3 (dictify
       'format uint16be
       'coverage uint16be ; pointer
       'entryExitCount uint16be
       'entryExitRecords (+Array EntryExitRecord 'entryExitCount))

    ;; MarkToBase Attachment Positioning
    4 (dictify
       'format uint16be
       'markCoverage uint16be ; pointer
       'baseCoverage uint16be ; pointer
       'classCount uint16be
       'markArray uint16be ; pointer
       'baseArray uint16be) ; pointer

    ;; MarkToLigature Attachment Positioning
    5 (dictify
       'format uint16be
       'markCoverage uint16be ; pointer
       'ligatureCoverage uint16be ; pointer
       'classCount uint16be
       'markArray uint16be ; pointer
       'ligatureArray uint16be)

    ;; MarkToMark Attachment Positioning
    6 (dictify
       'format uint16be
       'mark1Coverage uint16be ; pointer
       'mark2Coverage uint16be ; pointer
       'classCount uint16be
       'mark1Array uint16be ; pointer
       'mark2Array uint16be) ; pointer

    7 Context ;; Contextual positioning
    8 ChainingContext ;; Chaining contextual positioning

    ;; Extension positioning
    9 (dictify
       'posFormat uint16be
       'lookupType uint16be ;; cannot also be 9
       'extension uint32be) ; pointer
    )))

;; Fix circular reference
;; GPOSLookup.versions[9].extension.type = GPOSLookup;

(define gpos-common-dict (dictify 'scriptList uint16be ; pointer
                                  'featureList uint16be ; pointer
                                  'lookupList uint16be)) ; pointer

(define-subclass VersionedStruct (GPOS-VersionedStruct))
(define GPOS (+GPOS-VersionedStruct uint32be
                               (dictify
                                #x00010000 gpos-common-dict
                                #x00010001 (append gpos-common-dict (dictify 'featureVariations uint32be))))) ; pointer