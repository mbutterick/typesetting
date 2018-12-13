#lang racket/base
(require xenomorph/redo
         sugar/unstable/class
         sugar/unstable/dict
         "../helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/OS2.js
|#

(define OS/2 (let ()
               (define type-1
                 (dictify     'typoAscender       int16be
                              'typoDescender      int16be
                              'typoLineGap        int16be
                              'winAscent          uint16be
                              'winDescent         uint16be
                              'codePageRange      (+xarray #:type uint32be #:length 2)))

               (define type-2
                 (dictify     'xHeight            int16be
                              'capHeight          int16be
                              'defaultChar        uint16be
                              'breakChar          uint16be
                              'maxContent         uint16be))

               (define type-5
                 (dictify     'usLowerOpticalPointSize uint16be
                              'usUpperOpticalPointSize uint16be))

               (+xversioned-struct
                uint16be 
                (dictify
                 'header (dictify 'xAvgCharWidth          int16be   ;; average weighted advance width of lower case letters and space
                                  'usWeightClass          uint16be  ;; visual weight of stroke in glyphs
                                  'usWidthClass           uint16be  ;; relative change from the normal aspect ratio (width to height ratio)
                                  ;; Indicates font embedding licensing rights
                                  'fsType                 (+xbitfield #:type uint16be
                                                                      #:flags '(null noEmbedding viewOnly editable null  null null null noSubsetting bitmapOnly))
                                  'ySubscriptXSize        int16be   ;; recommended horizontal size in pixels for subscripts
                                  'ySubscriptYSize        int16be   ;; recommended vertical size in pixels for subscripts
                                  'ySubscriptXOffset      int16be   ;; recommended horizontal offset for subscripts
                                  'ySubscriptYOffset      int16be   ;; recommended vertical offset form the baseline for subscripts
                                  'ySuperscriptXSize      int16be   ;; recommended horizontal size in pixels for superscripts
                                  'ySuperscriptYSize      int16be   ;; recommended vertical size in pixels for superscripts
                                  'ySuperscriptXOffset    int16be   ;; recommended horizontal offset for superscripts
                                  'ySuperscriptYOffset    int16be   ;; recommended vertical offset from the baseline for superscripts
                                  'yStrikeoutSize         int16be   ;; width of the strikeout stroke
                                  'yStrikeoutPosition     int16be   ;; position of the strikeout stroke relative to the baseline
                                  'sFamilyClass           int16be   ;; classification of font-family design
                                  'panose                 (+xarray #:type uint8 #:length 10)   ;; describe the visual characteristics of a given typeface
                                  'ulCharRange            (+xarray #:type uint32be #:length 4)
                                  'vendorID               (+xsymbol #:length 4)          ;; four character identifier for the font vendor
                                  ;; bit field containing information about the font
                                  'fsSelection            (+xbitfield #:type uint16
                                                                      #:flags '(italic underscore negative outlined strikeout bold regular useTypoMetrics wws oblique))
                                  'usFirstCharIndex       uint16be  ;; The minimum Unicode index in this font
                                  'usLastCharIndex        uint16be)   ;; The maximum Unicode index in this font
                          
                 0 null
                 1 type-1
                 2 (append type-1 type-2)
                 3 (append type-1 type-2)
                 4 (append type-1 type-2)
                 5 (append type-1 type-2 type-5)))))

(module+ test
 (require rackunit racket/serialize sugar/unstable/js)
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables OS/2 offset))
 (define len (· dir tables OS/2 length))
 (check-equal? offset 360)
 (check-equal? len 96)
 (define ds (open-input-bytes (peek-bytes len offset ip)))
 (define version (decode uint16be ds))
 #;(send OS/2 force-version! version)
 #;(define table-data (send OS/2 decode ds))
 #;(check-equal? (· table-data panose) '(2 0 5 3 6 0 0 2 0 4))
 #;(check-equal? (· table-data sFamilyClass) 0))
