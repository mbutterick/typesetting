#lang fontkit/racket
(require xenomorph br/cond "opentype.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/GSUB.js
|#

(define Sequence (+Array uint16be uint16be))
(define AlternateSet Sequence)

(define Ligature (+Struct
                  (dictify
                   'glyph uint16be
                   'compCount uint16be
                   'components (+Array uint16be (λ (t) (sub1 (· t compCount)))))))

(define LigatureSet (+Array (+Pointer uint16be Ligature) uint16be))

(define-subclass VersionedStruct (GSUBLookup-VersionedStruct))
(define GSUBLookup
  (+GSUBLookup-VersionedStruct
   'lookupType
   (dictify
    ;; Single Substitution
    1 (+VersionedStruct uint16be
                        (dictify
                         1 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'deltaGlyphID int16be)
                         2 (dictify
                            'coverage (+Pointer uint16be Coverage)
                            'glyphCount uint16be
                            'substitute (+LazyArray uint16be 'glyphCount))))
    2 ;; Multiple Substitution
    (dictify
     'substFormat uint16be
     'coverage (+Pointer uint16be Coverage)
     'count uint16be
     'sequences (+LazyArray (+Pointer uint16be Sequence) 'count))

    3 ;; Alternate Substitution
    (dictify
     'substFormat uint16be
     'coverage (+Pointer uint16be Coverage)
     'count uint16be
     'alternateSet (+LazyArray (+Pointer uint16be AlternateSet) 'count))

    4 ;; Ligature Substitution
    (dictify
     'substFormat uint16be
     'coverage (+Pointer uint16be Coverage)
     'count uint16be
     'ligatureSets (+LazyArray (+Pointer uint16be LigatureSet) 'count))

    5 Context ;; Contextual Substitution
    6 ChainingContext ;; Chaining Contextual Substitution

    7 ;; Extension Substitution
    (dictify
     'substFormat uint16be
     'lookupType uint16be ; cannot also be 7
     'extension (+Pointer uint32be (λ () (error 'circular-reference-unfixed))))

    8 ;; Reverse Chaining Contextual Single Substitution
    (dictify
     'substFormat uint16be
     'coverage (+Pointer uint16be Coverage)
     'backTrackCoverage (+Array (+Pointer uint16be Coverage) 'backtrackGlyphCount)
     'lookaheadGlyphCount uint16be
     'lookaheadCoverage (+Array (+Pointer uint16be Coverage) 'lookaheadGlyphCount)
     'glyphCount uint16be
     'substitute (+Array uint16be 'glyphCount)))))

;; Fix circular reference
(ref*-set! GSUBLookup 'versions 7 'extension 'type GSUBLookup)

(define-subclass VersionedStruct (GSUB-MainVersionedStruct))
(define GSUB (+GSUB-MainVersionedStruct uint32be
                                        (dictify
                                         'header (dictify 'scriptList (+Pointer uint16be ScriptList)
                                                          'featureList (+Pointer uint16be FeatureList)
                                                          'lookupList (+Pointer uint16be (LookupList GSUBLookup))
                                                          )
                                         #x00010000 (dictify)
                                         #;#x00010001 #;(+Pointer uint32be FeatureVariations))))

(test-module)