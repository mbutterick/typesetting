#lang racket/base
(require (for-syntax racket/base)
         xenomorph
         racket/class
         racket/match
         racket/list
         racket/contract
         sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         sugar/unstable/stub
         "ffi/freetype.rkt"
         "helper.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

#|
/**
 * Glyph objects represent a glyph in the font. They have various properties for accessing metrics and
 * the actual vector path the glyph represents, and methods for rendering the glyph to a graphics context.
 *
 * You do not create glyph objects directly. They are created by various methods on the font object.
 * There are several subclasses of the base Glyph class internally that may be returned depending
 * on the font format, but they all inherit from this class.
 */
|#


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/Glyph.js
|#

(define-subclass object% (Glyph id codePoints font)
  (field [_font font]
         [isMark (andmap is-mark? codePoints)]
         [isLigature (> (length codePoints) 1)]
         [_metrics #f])

  (as-methods
   _getPath
   _getCBox
   _getBBox
   _getTableMetrics
   advanceWidth
   _getMetrics))

(define-stub-stop _getPath)
(define-stub-stop _getCBox)
(define-stub-stop _getBBox)
(define-stub-stop _getTableMetrics)

(define/contract (advanceWidth this)
  (->m number?)
  (hash-ref (_getMetrics this) 'advanceWidth))


(define/contract (_getMetrics this)
  (->m hash?)
  (unless (· this _metrics)
    (define face (· this _font ft-face))
    (FT_Load_Glyph face (· this id) FT_LOAD_NO_RECURSE)
    (define glyph (FT_FaceRec-glyph face))
    (define glyph-metrics (FT_GlyphSlotRec-metrics glyph))
    (set-field! _metrics this (mhash))
    (hash-set*! (· this _metrics)
                'advanceWidth (FT_Glyph_Metrics-horiAdvance glyph-metrics)
                'leftBearing (FT_Glyph_Metrics-horiBearingX glyph-metrics)))
  (· this _metrics))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/CFFGlyph.js
|#

(define-subclass Glyph (CFFGlyph)
  (error 'cff-glyph-unimplemented)

  #;(define/override (_getName this)
      (->m any/c)
      (if (send (· this _font) _getTable 'CFF2)
          (super _getName)
          (send (send (· this _font) _getTable 'CFF_) getGlyphName (· this id))))
  (as-methods
   #;_getName
   #;bias
   #;_getPath))



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
  (define/public (_parseGlyphCoord port prev short same)
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
  (define/public (_decode)
    (define offsets (· _font loca offsets))
    (match-define (list glyfPos nextPos) (take (drop offsets id) 2))

    ;; Nothing to do if there is no data for this glyph
    (and (not (= glyfPos nextPos))
         (let ()
           (define port (send _font _getTableStream 'glyf))
           (pos port (+ (pos port) glyfPos))
           (define startPos (pos port))
           (define glyph (decode GlyfHeader port))
           (match (· glyph numberOfContours)
             [(? positive?) (_decodeSimple glyph port)]
             [(? negative?) (_decodeComposite glyph port startPos)])
           glyph)))

  (define/public (_decodeSimple glyph port)
    (unless (dict? glyph)
      (raise-argument-error 'TTFGlyph-_decodeSimple "decoded RGlyfHeader" glyph))

    (unless (input-port? port)
      (raise-argument-error 'TTFGlyph-_decodeSimple "input port" port))

    ;; this is a simple glyph
    (dict-set! glyph 'points empty)
    (define endPtsOfContours (decode (+Array uint16be (· glyph numberOfContours)) port))
    (dict-set! glyph 'instructions (decode (+Array uint8be uint16be) port))
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
        (set-field! x point next-px)
        (set-field! y point next-py)
        (values (cons point points) next-px next-py)))
    (dict-set! glyph 'points (reverse points)))

  (define/public (_decodeComposite glyph port [offset 0])
    ;; this is a composite glyph
    (dict-set! glyph 'components empty)
    (define haveInstructions #f)
    (define flags MORE_COMPONENTS)

    (dict-set! glyph 'components
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
                         (set-field! pos component gPos)

                         (cond
                           [(not (zero? (bitwise-and flags WE_HAVE_A_SCALE)))
                            (define scale (read-fixed14 port))
                            (set-field! scaleX component scale)
                            (set-field! scaleY component scale)]
                           [(not (zero? (bitwise-and flags WE_HAVE_AN_X_AND_Y_SCALE)))
                            (set-field! scaleX component (read-fixed14 port))
                            (set-field! scaleY component (read-fixed14 port))]
                           [(not (zero? (bitwise-and flags WE_HAVE_A_TWO_BY_TWO)))
                            (set-field! scaleX component (read-fixed14 port))
                            (set-field! scale01 component (read-fixed14 port))
                            (set-field! scale10 component (read-fixed14 port))
                            (set-field! scaleY component (read-fixed14 port))])
                         component))
    haveInstructions))
        

(define (bytes->fixed14 b1 b2)
  (/ (+ (* b1 (expt 2 8)) b2) (expt 2 14) 1.0))
  
(define (read-fixed14 stream)
  (define b1 (send uint8 decode stream))
  (define b2 (send uint8 decode stream))
  (bytes->fixed14 b1 b2))
