#lang racket/base
(require "racket.rkt")

(require xenomorph)
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
                       (dictify 'tag (+Symbol 4)
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
                      (dictify 'tag (+Symbol 4)
                               'script (+ScriptRecord-Pointer uint16be Script (mhash 'type 'parent)))))

(define ScriptList (+Array ScriptRecord uint16be))

;;#######################
;; Features and Lookups #
;;#######################


(define Feature (+Struct (dictify
                          'featureParams uint16be
                          'lookupCount uint16be
                          'lookupListIndexes (+Array uint16be 'lookupCount))))

(define-subclass Struct (FeatureRec))
(define-subclass Pointer (FeatureRec-Pointer))
(define FeatureRecord (+FeatureRec (dictify
                                    'tag (+Symbol 4)
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

;;#########################
;; Class Definition Table #
;;#########################

(define ClassRangeRecord (+Struct
                          (dictify
                           'start uint16be
                           'end uint16be
                           'class uint16be)))

(define ClassDef (+VersionedStruct uint16be
                                   (dictify
                                    1 ;; Class array
                                    (dictify
                                     'startGlyph uint16be
                                     'glyphCount uint16be
                                     'classValueArray (+Array uint16be 'glyphCount))
                                    2 ;; Class ranges
                                    (dictify
                                     'classRangeCount uint16be
                                     'classRangeRecord (+Array ClassRangeRecord 'classRangeCount)))))


;;###############
;; Device Table #
;;###############

(define Device (+Struct
                (dictify
                 'startSize uint16be
                 'endSize uint16be
                 'deltaFormat uint16be)))

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
       'coverage (+Pointer uint16be Coverage)
       'ruleSetCount uint16be
       'ruleSets (+Array (+Pointer uint16be 'RuleSet) 'ruleSetCount))
    
    ;; Class-based context
    2 (dictify
       'coverage (+Pointer uint16be Coverage)
       'classDef (+Pointer uint16be 'ClassDef)
       'classSetCnt uint16be
       'classSet (+Array (+Pointer uint16be 'ClassSet) 'classSetCnt))

    3 (dictify
       'glyphCount uint16be
       'lookupCount uint16be
       'coverages (+Array (+Pointer uint16be Coverage) 'glyphCount)
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
       'coverage (+Pointer uint16be Coverage)
       'chainCount uint16be
       'chainRuleSets (+Array (+Pointer uint16be 'ChainRuleSet) 'chainCount))

    ;; Class-based chaining context
    2 (dictify
       'coverage (+Pointer uint16be Coverage)
       'backtrackClassDef (+Pointer uint16be 'ClassDef)
       'inputClassDef (+Pointer uint16be 'ClassDef)
       'lookaheadClassDef (+Pointer uint16be 'ClassDef)
       'chainCount uint16be
       'chainClassSet (+Array (+Pointer uint16be 'ChainRuleSet) 'chainCount))

    ;; Coverage-based chaining context
    3 (dictify
       'backtrackGlyphCount uint16be
       'backtrackCoverage (+Array (+Pointer uint16be Coverage) 'backtrackGlyphCount)
       'inputGlyphCount uint16be
       'inputCoverage (+Array (+Pointer uint16be Coverage) 'inputGlyphCount)
       'lookaheadGlyphCount uint16be
       'lookaheadCoverage (+Array (+Pointer uint16be Coverage) 'lookaheadGlyphCount)
       'lookupCount uint16be
       'lookupRecords (+Array LookupRecord 'lookupCount)))))
