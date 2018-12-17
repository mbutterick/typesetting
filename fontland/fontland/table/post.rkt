#lang racket/base
(require xenomorph
         sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         "../helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/post.js
|#

(define post (x:versioned-struct
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
                                                 'glyphNameIndex (x:array #:type uint16be #:length 'numberOfGlyphs)
                                                 'names (x:array (x:string #:length uint8))
                                                 )
                2.5 (dictify 'numberOfGlyphs uint16be
                                                   'offsets (x:array #:type uint8))
                3 null
                4 (dictify 'map (x:array #:type uint32be #:length (λ (t) (· t parent maxp numGlyphs)))))))

(module+ test
 (require rackunit racket/serialize racket/class)
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables post offset))
 (define len (· dir tables post length))
 (check-equal? offset 41520)
 (check-equal? len 514)
 (define ds (open-input-bytes (peek-bytes len offset ip)))
 (define version (decode fixed32be ds)) ; version = 2
 #|
(send post force-version! version)
 (define table-data (decode post ds))
 (check-equal? (· table-data underlineThickness) 58)
 (check-equal? (· table-data underlinePosition) -178)
 (check-equal? (· table-data names) '("periodcentered" "macron"))
|#
  )