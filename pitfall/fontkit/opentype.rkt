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
                                'langSys (+Pointer uint16be LangSysTable 'parent))))

(define Script (+Struct
                (dictify 'defaultLangSys (+Pointer uint16be LangSysTable)
                         'count uint16be
                         'langSysRecords (+Array LangSysRecord 'count))))

(define-subclass Struct (ScriptRecord-Struct))
(define ScriptRecord (+ScriptRecord-Struct
                      (dictify 'tag (+String 4)
                               'script (+Pointer uint16be Script 'parent))))

(define ScriptList (+Array ScriptRecord uint16be))

;;#######################
;; Features and Lookups #
;;#######################


(define Feature (+Struct (dictify
                                'featureParams uint16be ; pointer
                                'lookupCount uint16be
                                'lookupListIndexes (+Array uint16be 'lookupCount))))

(define FeatureRecord (+Struct (dictify
                                'tag (+String 4)
                                'feature (+Pointer uint16be Feature 'parent))))

(define FeatureList (+Array FeatureRecord uint16be))

(define LookupFlags (+Bitfield uint16be '(rightToLeft ignoreBaseGlyphs ignoreLigatures ignoreMarks useMarkFilteringSet #f markAttachmentType)))

(define (LookupList SubTable)
  (define Lookup (+Struct
                  (dictify
                   'lookupType uint16be
                   'flags LookupFlags
                   'subTableCount uint16be
                   'subTables (+Array (+Pointer uint16be SubTable) 'subTableCount)
                   'markFilteringSet uint16be))) ; TODO: only present when flags says so ...
  (+LazyArray (+Pointer uint16be Lookup) uint16be))

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
