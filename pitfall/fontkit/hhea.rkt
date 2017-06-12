#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

(define-subclass Struct (Rhhea))

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
                'reserved             (+Array int16be 4)
                'metricDataFormat     int16be   ;; 0 for current format
                'numberOfMetrics      uint16be   ;; Number of advance widths in 'hmtx' table
                )))



