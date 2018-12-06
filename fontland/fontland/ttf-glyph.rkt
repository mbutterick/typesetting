#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/class
         "glyph.rkt"
         sugar/unstable/dict
         sugar/unstable/js
         xenomorph
         racket/struct)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/TTFGlyph.js
|#


;; Represents a TrueType glyph.
#;(define-subclass Glyph (TTFGlyph)
    (inherit-field _font id))

(struct ttf-glyph glyph () #:transparent)

(define (+ttf-glyph . args)
  (apply +glyph #:constructor ttf-glyph args))

;; The header for both simple and composite glyphs
(define GlyfHeader (+Struct
                    (dictify 'numberOfContours int16be ;; if negative, this is a composite glyph
                             'xMin int16be
                             'yMin int16be
                             'xMax int16be
                             'yMax int16be)))

;; Flags for simple glyphs
(define-syntax (define-flag-series stx)
  (syntax-case stx ()
    [(_ . IDS)
     #`(match-define (list . IDS) (map (λ (x) (expt 2 x)) (range #,(length (syntax->list #'IDS)))))]))

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
(struct Point (onCurve endContour x y) #:transparent #:mutable)

(define (+Point onCurve endContour [x 0] [y 0])
  (Point onCurve endContour x y))

(define (copy pt)
  (apply +Point (struct->list pt)))

;; Represents a component in a composite glyph
(struct Component (glyphID dx dy pos scaleX scaleY scale01 scale10) #:transparent #:mutable)

(define (+Component glyphID dx dy [pos 0]
                    [scaleX 1]
                    [scaleY 1]
                    [scale01 0]
                    [scale10 0])
  (Component glyphID dx dy pos scaleX scaleY scale01 scale10))

  



;; Parses just the glyph header and returns the bounding box
#;(define/override (_getCBox internal)
    (unfinished))

;; Parses a single glyph coordinate
(define (_parseGlyphCoord port prev short same)
  (unless (input-port? port)
    (raise-argument-error '_parseGlyphCoord "input port" port))
  (unless (number? prev)
    (raise-argument-error '_parseGlyphCoord "number" prev))
  (unless (and (boolean? short) (boolean? same))
    (raise-argument-error '_parseGlyphCoord "booleans" (list short same)))
  (+ prev (if short 
              ((if (not same) - +) (decode uint8 port))
              (if same 0 (decode int16be port)))))
                 

;; Decodes the glyph data into points for simple glyphs,
;; or components for composite glyphs
(define (glyph-decode ttfg)
  (define offsets (· (glyph-_font ttfg) loca offsets))
  (match-define (list glyfPos nextPos) (take (drop offsets (glyph-id ttfg)) 2))

  ;; Nothing to do if there is no data for this glyph
  (and (not (= glyfPos nextPos))
       (let ()
         (define port (send (glyph-_font ttfg) _getTableStream 'glyf))
         (pos port (+ (pos port) glyfPos))
         (define startPos (pos port))
         (define glyph-data (decode GlyfHeader port))
         (match (· glyph-data numberOfContours)
           [(? positive?) (_decodeSimple glyph-data port)]
           [(? negative?) (_decodeComposite glyph-data port startPos)])
         glyph-data)))

(define (_decodeSimple glyph-data port)
  (unless (dict? glyph-data)
    (raise-argument-error 'TTFGlyph-_decodeSimple "decoded RGlyfHeader" glyph-data))

  (unless (input-port? port)
    (raise-argument-error 'TTFGlyph-_decodeSimple "input port" port))

  ;; this is a simple glyph
  (dict-set! glyph-data 'points empty)
  (define endPtsOfContours (decode (+Array uint16be (· glyph-data numberOfContours)) port))
  (dict-set! glyph-data 'instructions (decode (+Array uint8be uint16be) port))
  (define numCoords (add1 (last endPtsOfContours)))

  (define flags
    (for*/lists (flags)
                ([i (in-naturals)]
                 #:break (= (length flags) numCoords)
                 [flag (in-value (decode uint8 port))]
                 [count (in-range (add1 (if (not (zero? (bitwise-and flag REPEAT)))
                                            (decode uint8 port)
                                            0)))])
      flag))

  (match-define-values
    (points _ _)
    (for/fold ([points empty] [px 0] [py 0])
              ([(flag i) (in-indexed flags)])
      (define point (+Point (zero? (bitwise-and flag ON_CURVE)) (and (index-of endPtsOfContours i) #t) 0 0))
      (define next-px (_parseGlyphCoord port px (not (zero? (bitwise-and flag X_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_X)))))
      (define next-py (_parseGlyphCoord port py (not (zero? (bitwise-and flag Y_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_Y)))))
      (set-Point-x! point next-px)
      (set-Point-y! point next-py)
      (values (cons point points) next-px next-py)))
  (dict-set! glyph-data 'points (reverse points)))

(define (_decodeComposite glyph-data port [offset 0])
  ;; this is a composite glyph
  (dict-set! glyph-data 'components empty)
  (define haveInstructions #f)
  (define flags MORE_COMPONENTS)

  (dict-set! glyph-data 'components
             (for/list ([i (in-naturals)]
                        #:break (zero? (bitwise-and flags MORE_COMPONENTS)))
                       (set! flags (send uint16be decode port))
                       (define gPos (- (pos port) offset))
                       (define glyphID (send uint16be decode port))
                       (unless haveInstructions
                         (set! haveInstructions (not (zero? (bitwise-and flags WE_HAVE_INSTRUCTIONS)))))

                       (match-define
                         (list dx dy)
                         (let ([decoder (if (not (zero? (bitwise-and flags ARG_1_AND_2_ARE_WORDS))) int16be int8)])
                           (list (send decoder decode port) (send decoder decode port))))

                       (define component (+Component glyphID dx dy))
                       (set-Component-pos! component gPos)

                       (cond
                         [(not (zero? (bitwise-and flags WE_HAVE_A_SCALE)))
                          (define scale (read-fixed14 port))
                          (set-Component-scaleX! component scale)
                          (set-Component-scaleY! component scale)]
                         [(not (zero? (bitwise-and flags WE_HAVE_AN_X_AND_Y_SCALE)))
                          (set-Component-scaleX! component (read-fixed14 port))
                          (set-Component-scaleY! component (read-fixed14 port))]
                         [(not (zero? (bitwise-and flags WE_HAVE_A_TWO_BY_TWO)))
                          (set-Component-scaleX! component (read-fixed14 port))
                          (set-Component-scale01! component (read-fixed14 port))
                          (set-Component-scale10! component (read-fixed14 port))
                          (set-Component-scaleY! component (read-fixed14 port))])
                       component))
  haveInstructions)
        

(define (bytes->fixed14 b1 b2)
  (/ (+ (* b1 (expt 2 8)) b2) (expt 2 14) 1.0))
  
(define (read-fixed14 stream)
  (define b1 (send uint8 decode stream))
  (define b2 (send uint8 decode stream))
  (bytes->fixed14 b1 b2))