#lang fontkit/racket
(require xenomorph)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/post.js
|#


(define-subclass VersionedStruct (Rpost))

(define post (make-object Rpost
               fixed32be
               (dictify
                'header (dictify 'italicAngle        fixed32be ;; Italic angle in counter-clockwise degrees from the vertical.
                                 'underlinePosition  int16be   ;; Suggested distance of the top of the underline from the baseline
                                 'underlineThickness int16be   ;; Suggested values for the underline thickness
                                 'isFixedPitch       uint32be  ;; Whether the font is monospaced
                                 'minMemType42       uint32be  ;; Minimum memory usage when a TrueType font is downloaded as a Type 42 font
                                 'maxMemType42       uint32be  ;; Maximum memory usage when a TrueType font is downloaded as a Type 42 font
                                 'minMemType1        uint32be  ;; Minimum memory usage when a TrueType font is downloaded as a Type 1 font
                                 'maxMemType1        uint32be)   ;; Maximum memory usage when a TrueType font is downloaded as a Type 1 font
                          
                1 null
                2 (dictify 'numberOfGlyphs uint16be
                                                 'glyphNameIndex (+Array uint16be 'numberOfGlyphs)
                                                 'names (+Array (+String uint8))
                                                 )
                2.5 (dictify 'numberOfGlyphs uint16be
                                                   'offsets (+Array uint8))
                3 null
                4 (dictify 'map (+Array uint32be (λ (t) (· t parent maxp numGlyphs)))))))

(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables post offset))
 (define len (· dir tables post length))
 (check-equal? offset 41520)
 (check-equal? len 514)
 (define ds (open-input-bytes (peek-bytes len offset ip)))
 (define version (decode fixed32be ds)) ; version = 2
 (send post force-version! version)
 (define table-data (decode post ds))
 (check-equal? (· table-data underlineThickness) 58)
 (check-equal? (· table-data underlinePosition) -178)
 (check-equal? (· table-data names) '("periodcentered" "macron")))