#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/opentype.js
|#

;;########################
;; Scripts and Languages #
;;########################

(define-subclass Array (FeatIdxArray))

(define LangSysTable (+Struct
                      (dictify 'reserved uint16be
                               'reqFeatureIndex uint16be
                               'featureCount uint16be
                               'featureIndexes (+FeatIdxArray uint16be 'featureCount))))

(define-subclass Pointer (LSR-Pointer))
(define LangSysRecord (+Struct
                       (dictify 'tag (+String 4)
                                'langSys (+LSR-Pointer uint16be LangSysTable (mhash 'type 'parent)))))

(define-subclass Pointer (DLS-Pointer))
(define-subclass Array (DLS-Array))
(define Script (+Struct
                (dictify 'defaultLangSys (+DLS-Pointer uint16be LangSysTable)
                         'count uint16be
                         'langSysRecords (+DLS-Array LangSysRecord 'count))))

(define-subclass Struct (ScriptRecord-Struct))
(define-subclass Pointer (ScriptRecord-Pointer))
(define ScriptRecord (+ScriptRecord-Struct
                      (dictify 'tag (+String 4)
                               'script (+ScriptRecord-Pointer uint16be Script (mhash 'type 'parent)))))

(define ScriptList (+Array ScriptRecord uint16be))

;;#######################
;; Features and Lookups #
;;#######################


(define Feature (+Struct (dictify
                          'featureParams uint16be ; pointer
                          'lookupCount uint16be
                          'lookupListIndexes (+Array uint16be 'lookupCount))))

(define-subclass Struct (FeatureRec))
(define-subclass Pointer (FeatureRec-Pointer))
(define FeatureRecord (+FeatureRec (dictify
                                'tag (+String 4)
                                'feature (+FeatureRec-Pointer uint16be Feature (mhash 'type 'parent)))))

(define FeatureList (+Array FeatureRecord uint16be))

(define LookupFlags (+Bitfield uint16be '(rightToLeft ignoreBaseGlyphs ignoreLigatures ignoreMarks useMarkFilteringSet #f markAttachmentType)))

(define (LookupList SubTable)
  (define Lookup (+Struct
                  (dictify
                   'lookupType uint16be
                   'flags LookupFlags
                   'subTableCount uint16be
                   'subTables (+Array (+Pointer uint16be SubTable) 'subTableCount)
                   'markFilteringSet uint16be)))
  (+LazyArray (+Pointer uint16be Lookup) uint16be))


;;#################
;; Coverage Table #
;;#################

(define RangeRecord
  (+Struct
   (dictify
    'start              uint16be
    'end                uint16be
    'startCoverageIndex uint16be)))

(define Coverage
  (+VersionedStruct uint16be
                    (dictify
                     1 (dictify
                        'glyphCount   uint16be
                        'glyphs      (+Array uint16be 'glyphCount))

                     2 (dictify
                        'rangeCount   uint16be
                        'rangeRecords (+Array RangeRecord 'rangeCount)))))


;;#############################################
;; Contextual Substitution/Positioning Tables #
;;#############################################


(define LookupRecord (+Struct
                      (dictify
                       'sequenceIndex uint16be
                       'lookupListIndex uint16be)))

(define Context
  (+VersionedStruct
   uint16be
   (dictify
    ;; Simple context
    1 (dictify
       'coverage uint16be ; pointer
       'ruleSetCount uint16be
       'ruleSets (+Array uint16be 'ruleSetCount)) ; pointer

    ;; Class-based context
    2 (dictify
       'coverage uint16be ; pointer
       'classDef uint16be ; pointer
       'classSetCnt uint16be
       'classSet (+Array uint16be 'classSetCnt)) ; pointer

    3 (dictify
       'glyphCount uint16be
       'lookupCount uint16be
       'coverages (+Array uint16be 'glyphCount) ; pointer
       'lookupRecords (+Array LookupRecord 'lookupCount)))))


;;######################################################
;; Chaining Contextual Substitution/Positioning Tables #
;;######################################################

(define ChainingContext
  (+VersionedStruct
   uint16be
   (dictify
    ;; Simple context glyph substitution
    1 (dictify
       'coverage uint16be ; pointer
       'chainCount uint16be
       'chainRuelSets (+Array uint16be 'chainCount)) ; pointer

    ;; Class-based chaining context
    2 (dictify
       'coverage uint16be ; pointer
       'backtrackClassDef uint16be ; pointer
       'inputClassDef uint16be ; pointer
       'lookaheadClassDef uint16be ; pointer
       'chainCount uint16be
       'chainClassSet (+Array uint16be 'chainCount)) ; pointer

    ;; Coverage-based chaining context
    3 (dictify
       'backtrackGlyphCount uint16be
       'backtrackCoverage (+Array uint16be 'backtrackGlyphCount) ; pointer 
       'inputGlyphCount uint16be
       'inputCoverage (+Array uint16be 'inputGlyphCount) ; pointer
       'lookaheadGlyphCount uint16be
       'lookaheadCoverage (+Array uint16be 'lookaheadGlyphCount) ; pointer
       'lookupCount uint16be
       'lookupRecords (+Array LookupRecord 'lookupCount)))))
