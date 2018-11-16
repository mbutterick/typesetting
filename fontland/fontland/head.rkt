#lang racket/base
(require "racket.rkt")

(require xenomorph)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/head.js
|#

(define-subclass Struct (Rhead))

(define head (make-object Rhead
               (dictify
                'version            int32be                   ;; 0x00010000 (version 1.0)
                'revision           int32be                   ;; set by font manufacturer
                'checkSumAdjustment uint32be
                'magicNumber        uint32be                  ;; set to 0x5F0F3CF5
                'flags              uint16be
                'unitsPerEm         uint16be                  ;; range from 64 to 16384
                'created            (+Array int32be 2)
                'modified           (+Array int32be 2)
                'xMin               int16be                   ;; for all glyph bounding boxes
                'yMin               int16be                   ;; for all glyph bounding boxes
                'xMax               int16be                   ;; for all glyph bounding boxes
                'yMax               int16be                   ;; for all glyph bounding boxes
                'macStyle           (+Bitfield uint16be '(bold italic underline outline shadow condensed extended))
                'lowestRecPPEM      uint16be                  ;; smallest readable size in pixels
                'fontDirectionHint  int16be
                'indexToLocFormat   int16be                   ;; 0 for short offsets 1 for long
                'glyphDataFormat    int16be                   ;; 0 for current format
                )))


(test-module
 (require racket/serialize)
 (define ip (open-input-file charter-italic-path))
 (define dir (deserialize (read (open-input-file charter-italic-directory-path))))
 (define offset (· dir tables head offset))
 (define length (· dir tables head length))
 (check-equal? offset 236)
 (check-equal? length 54)
 (define table-bytes #"\0\1\0\0\0\2\0\0.\252t<_\17<\365\0\t\3\350\0\0\0\0\316\3\301\261\0\0\0\0\316\3\304\364\377\36\377\24\4\226\3\324\0\2\0\t\0\2\0\0\0\0")
 (set-port-position! ip 0)
 (check-equal? (peek-bytes length offset ip) table-bytes)
 (define table-data (send head decode table-bytes))
 (check-equal? (· table-data unitsPerEm) 1000)
 (check-equal? (· table-data yMin) -236)
 (check-equal? (· table-data yMax) 980)
 (check-equal? (· table-data xMax) 1174)
 (check-equal? (· table-data xMin) -226)
 (check-equal? (· table-data macStyle) (make-hasheq '((shadow . #f)
                                                      (extended . #f)
                                                      (condensed . #f)
                                                      (underline . #f)
                                                      (outline . #f)
                                                      (bold . #f)
                                                      (italic . #t))))
 (check-equal? (· table-data magicNumber) #x5F0F3CF5)
 (check-equal? (· table-data indexToLocFormat) 0) ; used in loca table
 (check-equal? (encode head table-data #f) table-bytes))