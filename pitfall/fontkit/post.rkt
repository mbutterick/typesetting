#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/post.js
|#


(define-subclass VersionedStruct (Rpost))

(define post (let ()
               (define header-fields
                 (dictify 'italicAngle        fixed32be ;; Italic angle in counter-clockwise degrees from the vertical.
                          'underlinePosition  int16be   ;; Suggested distance of the top of the underline from the baseline
                          'underlineThickness int16be   ;; Suggested values for the underline thickness
                          'isFixedPitch       uint32be  ;; Whether the font is monospaced
                          'minMemType42       uint32be  ;; Minimum memory usage when a TrueType font is downloaded as a Type 42 font
                          'maxMemType42       uint32be  ;; Maximum memory usage when a TrueType font is downloaded as a Type 42 font
                          'minMemType1        uint32be  ;; Minimum memory usage when a TrueType font is downloaded as a Type 1 font
                          'maxMemType1        uint32be   ;; Maximum memory usage when a TrueType font is downloaded as a Type 1 font
                          ))


               (make-object Rpost
                 fixed32be
                 (dictify
                  1 (append header-fields null)
                  2 (append header-fields (dictify 'numberOfGlyphs uint16be
                                                   'glyphNameIndex (+Array uint16be 'numberOfGlyphs)
                                                   ;; this field causes problems due to deficiency in String class
                                                   ;; 'names (+Array (+String uint8))
                                                   ))
                  2.5 (append header-fields (dictify 'numberOfGlyphs uint16be
                                                     'offsets (+Array uint8)))
                  3 (append header-fields null)
                  4 (append header-fields (dictify 'map (+Array uint32be (λ (t) (· (send (· t parent) _getTable 'maxp) numGlyphs)))))))))

(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables post offset))
 (define len (· dir tables post length))
 (check-equal? offset 41520)
 (check-equal? len 514)
 (define ds (+DecodeStream (peek-bytes len offset ip)))
 (define version (send fixed32be decode ds))
 (send post force-version! version)
 (define table-data (send post decode ds))
 (check-equal? (· table-data underlineThickness) 58)
 (check-equal? (· table-data underlinePosition) -178))