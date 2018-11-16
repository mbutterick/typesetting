#lang racket/base
(require "racket.rkt")

(require xenomorph)
(provide (all-defined-out))

(define-subclass Struct (Rmaxp))

(define maxp (make-object Rmaxp
               (dictify 'version                int32be
                        'numGlyphs              uint16be  ;; The number of glyphs in the font
                        'maxPoints              uint16be  ;; Maximum points in a non-composite glyph
                        'maxContours            uint16be  ;; Maximum contours in a non-composite glyph
                        'maxComponentPoints     uint16be  ;; Maximum points in a composite glyph
                        'maxComponentContours   uint16be  ;; Maximum contours in a composite glyph
                        'maxZones               uint16be  ;; 1 if instructions do not use the twilight zone, 2 otherwise
                        'maxTwilightPoints      uint16be  ;; Maximum points used in Z0
                        'maxStorage             uint16be  ;; Number of Storage Area locations
                        'maxFunctionDefs        uint16be  ;; Number of FDEFs
                        'maxInstructionDefs     uint16be  ;; Number of IDEFs
                        'maxStackElements       uint16be  ;; Maximum stack depth
                        'maxSizeOfInstructions  uint16be  ;; Maximum byte count for glyph instructions
                        'maxComponentElements   uint16be  ;; Maximum number of components referenced at “top level” for any composite glyph
                        'maxComponentDepth      uint16be  ;; Maximum levels of recursion; 1 for simple components
                        )))


(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define maxp-offset (· dir tables maxp offset))
 (define maxp-length (· dir tables maxp length))
 (check-equal? maxp-offset 328)
 (check-equal? maxp-length 32)
 (define maxp-bytes #"\0\1\0\0\0\345\0f\0\a\0O\0\4\0\1\0\0\0\0\0\n\0\0\2\0\1s\0\2\0\1")
 (set-port-position! ip 0)
 (check-equal? (peek-bytes maxp-length maxp-offset ip) maxp-bytes)
 (define maxp-data (send maxp decode maxp-bytes))
 (check-equal? (· maxp-data numGlyphs) 229)
 (check-equal? (· maxp-data version) 65536)) 