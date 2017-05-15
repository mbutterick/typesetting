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
     text)))

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
  (string? number? number? hash? procedure? . ->m . object?)
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


(define (text this text-string [x 0] [y 0] [options (mhash)])
  (send this _text text-string x y options (curry _line this)))


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
  (define lineGap (or (· options lineGap) (· this _lineGap) 0))
  (if (not wrapper)
      (increment-field! x this (send this widthOfString text))
      (increment-field! y (+ (send this currentLineHeight #t) lineGap))))


(define/contract (_fragment this text x y options)
  (string? number? number? hash? . ->m . void?)
  (error '_fragment))

