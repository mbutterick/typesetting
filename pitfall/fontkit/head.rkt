#lang fontkit/racket
(require restructure)
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



