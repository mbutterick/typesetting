#lang fontkit/racket
(require "glyph.rkt" restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/TTFGlyph.js
|#

;; The header for both simple and composite glyphs
(define-subclass Struct (RGlyfHeader))
(define GlyfHeader (+RGlyfHeader
                    (dictify 'numberOfContours int16be ;; if negative, this is a composite glyph
                             'xMin int16be
                             'yMin int16be
                             'xMax int16be
                             'yMax int16be)))

;; Flags for simple glyphs
(define-macro (define-flag-series . IDS)
  #`(match-define (list . IDS) (map (curry expt 2) (range #,(length (syntax->list #'IDS))))))

;; Flags for simple glyphs
(define-flag-series ON_CURVE
  X_SHORT_VECTOR
  Y_SHORT_VECTOR
  REPEAT
  SAME_X
  SAME_Y)

;; Flags for composite glyphs
(define-flag-series ARG_1_AND_2_ARE_WORDS    
  ARGS_ARE_XY_VALUES       
  ROUND_XY_TO_GRID         
  WE_HAVE_A_SCALE
  __EMPTY-FLAG___
  MORE_COMPONENTS          
  WE_HAVE_AN_X_AND_Y_SCALE 
  WE_HAVE_A_TWO_BY_TWO     
  WE_HAVE_INSTRUCTIONS     
  USE_MY_METRICS           
  OVERLAP_COMPOUND         
  SCALED_COMPONENT_OFFSET  
  UNSCALED_COMPONENT_OFFSET)

;; Represents a point in a simple glyph
(define-subclass object% (Point onCurve endContour [x 0] [y 0])

  (define/public (copy)
    (+Point onCurve endContour x y)))

;; Represents a component in a composite glyph
(define-subclass object% (Component glyphID dx dy)
  (field [pos 0]
         [scaleX 1]
         [scaleY 1]
         [scale01 0]
         [scale10 0]))

;; Represents a TrueType glyph.
(define-subclass Glyph (TTFGlyph)
  (inherit-field _font id)

  ;; Parses just the glyph header and returns the bounding box
  (define/override (_getCBox internal)
    (unfinished))

  ;; Parses a single glyph coordinate
  (define/public (_parseGlyphCoord stream prev short same)
    (unless (DecodeStream? stream)
      (raise-argument-error '_parseGlyphCoord "DecodeStream" stream))
    (unless (number? prev)
      (raise-argument-error '_parseGlyphCoord "number" prev))
    (unless (and (boolean? short) (boolean? same))
      (raise-argument-error '_parseGlyphCoord "booleans" (list short same)))
    (+ prev (if short 
                ((if (not same) - +) (send uint8 decode stream))
                (if same 0 (send int16be decode stream)))))
                 

  ;; Decodes the glyph data into points for simple glyphs,
  ;; or components for composite glyphs
  (define/public (_decode)
    (define offsets (· _font loca offsets))
    (match-define (list glyfPos nextPos) (take (drop offsets id) 2))

    ;; Nothing to do if there is no data for this glyph
    (and (not (= glyfPos nextPos))
         (let ()
           (define stream (send _font _getTableStream 'glyf))
           (send stream pos (+ (send stream pos) glyfPos))
           (define startPos (· stream pos))
           (define glyph (send GlyfHeader decode stream))
           (match (· glyph numberOfContours)
             [(? positive?) (_decodeSimple glyph stream)]
             [(? negative?) (_decodeComposite glyph stream startPos)])
           glyph)))

  (define/public (_decodeSimple glyph stream)
    (unless (hash? glyph)
      (raise-argument-error 'TTFGlyph-_decodeSimple "decoded RGlyfHeader" glyph))

    (unless (DecodeStream? stream)
      (raise-argument-error 'TTFGlyph-_decodeSimple "DecodeStream" stream))

    ;; this is a simple glyph
    (hash-set! glyph 'points empty)
    (define endPtsOfContours (send (+Array uint16be (· glyph numberOfContours)) decode stream))
    (hash-set! glyph 'instructions (send (+Array uint8be uint16be) decode stream))
    (define numCoords (add1 (last endPtsOfContours)))

    (define flags
      (for*/lists (flags)
                  ([i (in-naturals)]
                   #:break (= (length flags) numCoords)
                   [flag (in-value (send uint8 decode stream))]
                   [count (in-range (add1 (if (not (zero? (bitwise-and flag REPEAT)))
                                              (send uint8 decode stream)
                                              0)))])
                  flag))

    (match-define-values
     (points _ _)
     (for/fold ([points empty] [px 0] [py 0])
               ([(flag i) (in-indexed flags)])
       (define point (+Point (zero? (bitwise-and flag ON_CURVE)) (and (index-of endPtsOfContours i) #t) 0 0))
       (define next-px (_parseGlyphCoord stream px (not (zero? (bitwise-and flag X_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_X)))))
       (define next-py (_parseGlyphCoord stream py (not (zero? (bitwise-and flag Y_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_Y)))))
       (set-field! x point next-px)
       (set-field! y point next-py)
       (values (cons point points) next-px next-py)))
    (hash-set! glyph 'points (reverse points)))

  (define/public (_decodeComposite glyph stream [offset 0])
    ;; this is a composite glyph
    (hash-set! glyph 'components empty)
    (define haveInstructions #f)
    (define flags MORE_COMPONENTS)

    (hash-set! glyph 'components
               (for/list ([i (in-naturals)]
                          #:break (zero? (bitwise-and flags MORE_COMPONENTS)))
                 (set! flags (send uint16be decode stream))
                 (define gPos (- (send stream pos) offset))
                 (define glyphID (send uint16be decode stream))
                 (unless haveInstructions
                   (set! haveInstructions (not (zero? (bitwise-and flags WE_HAVE_INSTRUCTIONS)))))

                 (match-define
                   (list dx dy)
                   (let ([decoder (if (not (zero? (bitwise-and flags ARG_1_AND_2_ARE_WORDS))) int16be int8)])
                     (list (send decoder decode stream) (send decoder decode stream))))

                 (define component (+Component glyphID dx dy))
                 (set-field! pos component gPos)

                 (cond
                   [(not (zero? (bitwise-and flags WE_HAVE_A_SCALE)))
                    (define scale (read-fixed14 stream))
                    (set-field! scaleX component scale)
                    (set-field! scaleY component scale)]
                   [(not (zero? (bitwise-and flags WE_HAVE_AN_X_AND_Y_SCALE)))
                    (set-field! scaleX component (read-fixed14 stream))
                    (set-field! scaleY component (read-fixed14 stream))]
                   [(not (zero? (bitwise-and flags WE_HAVE_A_TWO_BY_TWO)))
                    (set-field! scaleX component (read-fixed14 stream))
                    (set-field! scale01 component (read-fixed14 stream))
                    (set-field! scale10 component (read-fixed14 stream))
                    (set-field! scaleY component (read-fixed14 stream))])
                 component))
    haveInstructions))
        

(define (bytes->fixed14 b1 b2)
  (/ (+ (* b1 (expt 2 8)) b2) (expt 2 14) 1.0))
  
(define (read-fixed14 stream)
  (define b1 (send uint8 decode stream))
  (define b2 (send uint8 decode stream))
  (bytes->fixed14 b1 b2))