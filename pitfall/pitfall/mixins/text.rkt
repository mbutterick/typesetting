#lang pitfall/racket
(require sugar/list)
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
  
  (let* ([options (send this _initOptions options x y)]
         [text (format "~a" text)] ;; Convert text to a string
         ;; if the wordSpacing option is specified, remove multiple consecutive spaces
         [text (if (hash-ref options 'wordSpacing #f)
                   (string-replace text #px"\\s{2,}" " ")
                   text)])

    ;; word wrapping
    (cond
      #;[(hash-ref options 'width #f) (error 'unimplemented-branch-of-_text)] ; todo
      [else ; render paragraphs as single lines
       (for ([line (in-list (string-split text "\n"))])
         (lineCallback line options))]))
  
  this)


(define (text this text-string [x #f] [y #f] [options (mhash)])
  (send this _text text-string x y options (curry _line this)))


(define/contract (widthOfString this str [options (mhash)])
  ((string?) (hash?) . ->*m . number?)
  (+ (send (· this _font) widthOfString str (· this _fontSize) (hash-ref options 'features #f))
     (* (hash-ref options 'characterSpacing 0) (sub1 (string-length str)))))


(define/contract (_initOptions this [options (mhash)] [x #f] [y #f])
  (() (hash? (or/c number? #f) (or/c number? #f)) . ->*m . hash?)

  ;; clone options object
  (let ([options (hash-copy options)])

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

    options))
  

(define/contract (_line this text [options (mhash)] [wrapper #f])
  ((string?) (hash? (or/c procedure? #f)) . ->*m . void?)
  (send this _fragment text (· this x) (· this y) options)
  (define lineGap (or (hash-ref options 'lineGap #f) (· this _lineGap) 0))
  (if (not wrapper)
      (increment-field! x this (send this widthOfString text))
      (increment-field! y (+ (send this currentLineHeight #t) lineGap)))
  (void))


(define/contract (_fragment this text x y-in options)
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
  (define y (- (· this page height) y-in (* (/ (· this _font ascender) 1000) (· this _fontSize))))

  ;; add current font to page if necessary
  (hash-ref! (· this page fonts) (· this _font id) (λ () (· this _font ref)))
  
  ;; begin the text object
  (send this addContent "BT")

  ;; text position
  (send this addContent (format "1 0 0 1 ~a ~a Tm" (number x) (number y)))

  ;; font and font size
  (send this addContent (format "/~a ~a Tf" (· this _font id) (number (· this _fontSize))))

  ;; rendering mode
  (let ([mode (cond
                [(and (hash-ref options 'fill #f) (hash-ref options 'stroke #f)) 2]
                [(hash-ref options 'stroke #f) 1]
                [else 0])])
    (when (and mode (not (zero? mode)))
      (send this addContent (format "~a Tr" mode))))

  ;; Character spacing
  (when (not (zero? characterSpacing))
    (send this addContent (format "~a Tc" characterSpacing)))

  ;; Add the actual text
  ;; If we have a word spacing value, we need to encode each word separately
  ;; since the normal Tw operator only works on character code 32, which isn't
  ;; used for embedded fonts.
  (match-define (list encoded positions)
    (cond
      [(not (zero? wordSpacing)) (error 'unimplemented-brach)] ; todo
      [else (send (· this _font) encode text (hash-ref options 'features #f))]))

  (define scale (/ (· this _fontSize) 1000.0))
  (define commands empty)
  (define last 0)
  
  ;; Adds a segment of text to the TJ command buffer
  (define (addSegment cur)
    (when (< last cur)
      (let* ([hex (string-append* (sublist encoded last cur))]
             [posn (list-ref positions (sub1 cur))]
             [advance (- (· posn xAdvance) (· posn advanceWidth))])
        (push-end! commands (format "<~a> ~a" hex (number (- advance))))))
    (set! last cur))
        

  ;; Flushes the current TJ commands to the output stream
  (define (flush i)
    (addSegment i)
    (when (positive? (length commands))
      (send this addContent (format "[~a] TJ" (string-join commands " ")))
      (set! commands empty)))

  
  (for/fold ([hadOffset #f] [x x])
            ([(posn i) (in-indexed positions)])
    (define havingOffset
      (cond
        ;; If we have an x or y offset, we have to break out of the current TJ command
        ;; so we can move the text position.
        [(or (not (zero? (· posn xOffset))) (not (zero? (· posn yOffset))))
         ;; Flush the current buffer
         (flush i)
         ;; Move the text position and flush just the current character
         (send this addContent (format "1 0 0 1 ~a ~a Tm"
                                       (number (+ x (* (· posn xOffset) scale)))
                                       (number (+ y (* (· posn yOffset) scale)))))
         (flush (add1 i))
         #t]
        [else
         ;; If the last character had an offset, reset the text position
         (when hadOffset
           (send this addContent (format "1 0 0 1 ~a ~a Tm" (number x) (number y))))

         ;; Group segments that don't have any advance adjustments
         (unless (zero? (- (· posn xAdvance) (· posn advanceWidth)))
           (addSegment (add1 i)))

         #f]))

    (values havingOffset (+ x (* (· posn xAdvance) scale))))
  
  
  ;; Flush any remaining commands
  (let ([i (length positions)])
    (flush i))
  
  ;; end the text object
  (send this addContent "ET")

  ;; restore flipped coordinate system
  (send this restore)
  (void))

