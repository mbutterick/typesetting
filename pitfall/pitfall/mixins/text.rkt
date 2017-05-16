#lang pitfall/racket
(provide text-mixin)

(define (text-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [_lineGap #f]
           [_textOptions #f])

    (as-methods
     initText
     _initOptions
     _text
     _fragment
     text
     widthOfString)))

(define/contract (initText this)
  (->m void?)
  (set-field! x this 0)
  (set-field! y this 0)
  (lineGap this 0)
  (void))


(define/contract (lineGap this _lineGap)
  (number? . ->m . object?)
  (set-field! _lineGap this _lineGap)
  this)


(define/contract (moveDown this [lines 1] #:factor [factor 1])
  (() (number? #:factor number?) . ->*m . object?)
  (increment-field! y this (* factor (send this currentLineHeight #t) (+ lines (· this _lineGap))))
  this)


(define/contract (moveUp this [lines 1])
  (() (number?) . ->*m . object?)
  (moveDown this #:factor -1))


(define/contract (_text this text x y options lineCallback)
  (string? (or/c number? #f) (or/c number? #f) hash? procedure? . ->m . object?)
  (set! options (send this _initOptions options x y))

  ;; Convert text to a string
  ;; q: what else might it be?
  (set! text (format "~a" text))

  ;; if the wordSpacing option is specified, remove multiple consecutive spaces
  (when (hash-ref options 'wordSpacing #f)
    (set! text (string-replace text #px"\\s{2,}" " ")))

  ;; word wrapping
  (cond
    #;[(hash-ref options 'width #f)

       ] ; todo
    [else ; render paragraphs as single lines
     (for ([line (in-list (string-split text "\n"))])
       (lineCallback line options))])
  
  this)


(define (text this text-string [x #f] [y #f] [options (mhash)])
  (send this _text text-string x y options (curry _line this)))


(define/contract (widthOfString this string [options (mhash)])
  ((string?) (hash?) . ->*m . number?)
  42 ; todo
  )


(define/contract (_initOptions this [options (mhash)] [x #f] [y #f])
  (() (hash? (or/c number? #f) (or/c number? #f)) . ->*m . hash?)

  ;; clone options object
  (set! options (hash-copy options))

  ;; extend options with previous values for continued text
  (when (· this _textOptions)
    (for ([(key val) (in-hash (· this _textOptions))]
          #:unless (equal? (key "continued")))
      (hash-ref! options key val)))

  ;; Update the current position
  (when x (set-field! x this x))
  (when y (set-field! y this y))

  ;; wrap to margins if no x or y position passed
  (unless (not (hash-ref options 'lineBreak #t))
    (define margins (· this page margins))
    (hash-ref! options 'width (λ () (- (· this page width) (· this x) (· margins right)))))

  (hash-ref! options 'columns 0)
  (hash-ref! options 'columnGap 18) ; 1/4 inch in PS points

  options)
  

(define/contract (_line this text [options (mhash)] [wrapper #f])
  ((string?) (hash? (or/c procedure? #f)) . ->*m . void?)
  (send this _fragment text (· this x) (· this y) options)
  (define lineGap (or (hash-ref options 'lineGap #f) (· this _lineGap) 0))
  (if (not wrapper)
      (increment-field! x this (send this widthOfString text))
      (increment-field! y (+ (send this currentLineHeight #t) lineGap)))
  (void))


(define/contract (_fragment this text x y options)
  (string? number? number? hash? . ->m . void?)

  (define align (hash-ref options 'align 'left))
  (define wordSpacing (hash-ref options 'wordSpacing 0))
  (define characterSpacing (hash-ref options 'characterSpacing 0))

  ;;  text alignments ; todo

  ;; calculate the actual rendered width of the string after word and character spacing ; todo

  ;; create link annotations if the link option is given ; todo

  ;; create underline or strikethrough line ; todo

  ;; flip coordinate system
  (send this save)
  (send this transform 1 0 0 -1 0 (· this page height))
  (set! y (- (· this page height) y (* (/ (· this _font ascender) 1000) (· this _fontSize))))

  ;; add current font to page if necessary
  (hash-ref! (· this page fonts) (· this _font id) (λ () "a font ref" (· this _font ref)))
  
  ;; begin the text object
  (send this addContent "BT")

  ;; text position
  (send this addContent (format "1 0 0 1 ~a ~a Tm" (number x) (number y)))

  ;; font and font size ; todo

  ;; rendering mode
  (define mode (cond
                 [(and (hash-ref options 'fill #f) (hash-ref options 'stroke #f)) 2]
                 [(hash-ref options 'stroke #f) 1]
                 [else 0]))
  (when (and mode (not (zero? mode)))
    (send this addContent (format "~a Tr" mode)))

  ;; Character spacing
  (when (and characterSpacing (not (zero? characterSpacing)))
    (send this addContent (format "~a Tc" characterSpacing)))

  ;; Add the actual text
  ;; If we have a word spacing value, we need to encode each word separately
  ;; since the normal Tw operator only works on character code 32, which isn't
  ;; used for embedded fonts.
  ;; todo

  ;; Adds a segment of text to the TJ command buffer ; todo

  ;; Flushes the current TJ commands to the output stream ; todo

  ;; Flush any remaining commands ; todo
  
  ;; end the text object
  (send this addContent "ET")

  ;; restore flipped coordinate system
  (send this restore)
  (void))

