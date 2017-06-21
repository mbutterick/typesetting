#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

;;########################
;; Scripts and Languages #
;;########################

(define LangSysTable (+Struct
                      (dictify 'reserved uint16be
                               'reqFeatureIndex uint16be
                               'featureCount uint16be
                               'featureIndexes (+Array uint16be 'featureCount))))

(define LangSysRecord (+Struct
                       (dictify 'tag (+String 4)
                                'langSys (+Pointer uint16be LangSysTable (mhash 'type 'parent)))))

(define Script (+Struct
                (dictify 'defaultLangSys (+Pointer uint16be LangSysTable)
                         'count uint16be
                         'langSysRecords (+Array LangSysRecord 'count))))

(define-subclass Struct (ScriptRecord-Struct))
(define ScriptRecord (+ScriptRecord-Struct
                      (dictify 'tag (+String 4)
                               'script uint16be #;(+Pointer uint16be Script (mhash 'type 'parent)))))
(define-subclass Array (ScriptRecordArray)
  (define/override (decode stream ctx)
    (define val (super decode stream ctx))
    (report (· this _len))
    val))
(define ScriptList (+ScriptRecordArray ScriptRecord uint16be))

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
