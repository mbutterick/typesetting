#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

(define-subclass RStruct (Rhead))

(define head (make-object Rhead
               (dictify
                'version            int32be                   ;; 0x00010000 (version 1.0)
                'revision           int32be                   ;; set by font manufacturer
                'checkSumAdjustment uint32be
                'magicNumber        uint32be                  ;; set to 0x5F0F3CF5
                'flags              uint16be
                'unitsPerEm         uint16be                  ;; range from 64 to 16384
                'created            (make-object RArray int32be 2)
                'modified           (make-object RArray int32be 2)
                'xMin               int16be                   ;; for all glyph bounding boxes
                'yMin               int16be                   ;; for all glyph bounding boxes
                'xMax               int16be                   ;; for all glyph bounding boxes
                'yMax               int16be                   ;; for all glyph bounding boxes
                'macStyle           uint16be
                #|
new Bitfield(uint16be [
                                                           '  'bold' 'italic' 'underline' 'outline'
                                                                   '  'shadow' 'condensed' 'extended'
                                                                   '])
|#
                'lowestRecPPEM      uint16be                  ;; smallest readable size in pixels
                'fontDirectionHint int16be
                'indexToLocFormat   int16be                   ;; 0 for short offsets 1 for long
                'glyphDataFormat    int16be                    ;; 0 for current format
                )))

(test-module
 (require "directory.rkt")
 (define ip (open-input-file charter-path))
 (define dir (directory-decode ip))
 (define offset (· dir tables head offset))
 (define length (· dir tables head length))
 (check-equal? offset 236)
 (check-equal? length 54)
 (define table-bytes #"\0\1\0\0\0\2\0\0@\247\22 _\17<\365\0\t\3\350\0\0\0\0\316\3\301?\0\0\0\0\316\3\304\363\377_\377\24\4\251\3\303\0\0\0\t\0\2\0\0\0\0")
 (set-port-position! ip 0)
 (check-equal? (peek-bytes length offset ip) table-bytes)
 (define table-data (send head decode (make-object RDecodeStream table-bytes)))
 (check-equal? (· table-data unitsPerEm) 1000)
 (check-equal? (· table-data yMin) -236)
 (check-equal? (· table-data yMax) 963)
 (check-equal? (· table-data xMax) 1193)
 (check-equal? (· table-data xMin) -161)
 (check-equal? (· table-data magicNumber) #x5F0F3CF5))

