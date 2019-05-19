#lang debug racket/base
(require xenomorph
         racket/date)
(provide head)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/head.js
|#

; `name` table dates are seconds since 1/1/1904 (Mac file convention)
; whereas Racket / posix tracks since 1/1/1970
; so we adjust with the difference in seconds
(define mac-to-posix-delta 2082844800)

(define hfs-seconds (x:int #:size 8
                           #:signed #f
                           #:endian 'be
                           #:post-decode (λ (int) (seconds->date (- int mac-to-posix-delta) #f))
                           #:pre-encode (λ (dt) (+ (date->seconds dt) mac-to-posix-delta))))

(define head (x:struct
              'version            int32be                   ;; 0x00010000 (version 1.0)
              'revision           int32be                   ;; set by font manufacturer
              'checkSumAdjustment uint32be
              'magicNumber        uint32be                  ;; set to 0x5F0F3CF5
              'flags              uint16be
              'unitsPerEm         uint16be                  ;; range from 64 to 16384
              'created            hfs-seconds
              'modified           hfs-seconds
              'xMin               int16be                   ;; for all glyph bounding boxes
              'yMin               int16be                   ;; for all glyph bounding boxes
              'xMax               int16be                   ;; for all glyph bounding boxes
              'yMax               int16be                   ;; for all glyph bounding boxes
              'macStyle           (x:bitfield #:type uint16be
                                              #:flags '(bold italic underline outline shadow condensed extended))
              'lowestRecPPEM      uint16be                  ;; smallest readable size in pixels
              'fontDirectionHint  int16be
              'indexToLocFormat   int16be                   ;; 0 for short offsets 1 for long
              'glyphDataFormat    int16be                   ;; 0 for current format
              ))


(module+ test
  (require rackunit "../helper.rkt"
           racket/serialize)
  (define ip (open-input-file charter-italic-path))
  (define dir (deserialize (read (open-input-file charter-italic-directory-path))))
  (define offset (hash-ref (hash-ref (hash-ref dir 'tables) 'head) 'offset))
  (define length (hash-ref (hash-ref (hash-ref dir 'tables) 'head) 'length))
  (check-equal? offset 236)
  (check-equal? length 54)
  (define table-bytes #"\0\1\0\0\0\2\0\0.\252t<_\17<\365\0\t\3\350\0\0\0\0\316\3\301\261\0\0\0\0\316\3\304\364\377\36\377\24\4\226\3\324\0\2\0\t\0\2\0\0\0\0")
  (file-position ip 0)
  (check-equal? (peek-bytes length offset ip) table-bytes)
  (define table-data (decode head table-bytes))
  (check-equal? (hash-ref table-data 'unitsPerEm) 1000)
  (check-equal? (hash-ref table-data 'yMin) -236)
  (check-equal? (hash-ref table-data 'yMax) 980)
  (check-equal? (hash-ref table-data 'xMax) 1174)
  (check-equal? (hash-ref table-data 'xMin) -226)
  (check-equal? (hash-ref table-data 'macStyle) (make-hash '((shadow . #f)
                                                             (extended . #f)
                                                             (condensed . #f)
                                                             (underline . #f)
                                                             (outline . #f)
                                                             (bold . #f)
                                                             (italic . #t))))
  (check-equal? (hash-ref table-data 'magicNumber) #x5F0F3CF5)
  (check-equal? (hash-ref table-data 'indexToLocFormat) 0) ; used in loca table
  (check-equal? (encode head table-data #f) table-bytes))