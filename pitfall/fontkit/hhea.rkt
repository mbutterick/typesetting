#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

(define-subclass RStruct (Rhhea))

(define hhea (make-object Rhhea
               (dictify
                'version              int32be
                'ascent               int16be   ;; Distance from baseline of highest ascender
                'descent              int16be   ;; Distance from baseline of lowest descender
                'lineGap              int16be   ;; Typographic line gap
                'advanceWidthMax      uint16be  ;; Maximum advance width value in 'hmtx' table
                'minLeftSideBearing   int16be   ;; Maximum advance width value in 'hmtx' table
                'minRightSideBearing  int16be   ;; Minimum right sidebearing value
                'xMaxExtent           int16be
                'caretSlopeRise       int16be   ;; Used to calculate the slope of the cursor (rise/run); 1 for vertical
                'caretSlopeRun        int16be   ;; 0 for vertical
                'caretOffset          int16be   ;; Set to 0 for non-slanted fonts
                'reserved             (make-object RArray int16be 4)
                'metricDataFormat     int16be   ;; 0 for current format
                'numberOfMetrics      uint16be   ;; Number of advance widths in 'hmtx' table
                )))

(test-module
 (require "directory.rkt")
 (define ip (open-input-file charter-path))
 (define dir (directory-decode ip))
 (define offset (· dir tables hhea offset))
 (define length (· dir tables hhea length))
 (check-equal? offset 292)
 (check-equal? length 36)
 (define table-bytes #"\0\1\0\0\3\324\377\22\0\0\4\311\377_\377`\4\251\0\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\345")
 (set-port-position! ip 0)
 (check-equal? (peek-bytes length offset ip) table-bytes)
 (define table-data (send hhea decode (make-object RDecodeStream table-bytes)))
 (check-equal? (· table-data ascent) 980)
 (check-equal? (· table-data descent) -238))

