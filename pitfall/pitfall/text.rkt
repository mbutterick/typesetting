#lang racket/base
(require
  "core.rkt"
  "page.rkt"
  "annotations.rkt"
  "fonts.rkt"
  "vector.rkt"
  "color.rkt"
  racket/class
  racket/match
  racket/string
  racket/list
  sugar/unstable/class
  sugar/unstable/dict
  sugar/list
  racket/promise
  fontland/glyph-position)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/mixins/text.coffee
|#

(define (text doc str [x #f] [y #f] [options (mhash)])
  (when x (set-$doc-x! doc x))
  (when y (set-$doc-y! doc y))
  (line doc str options)
  doc)

(define (fragment doc text x y-in options)
  (define character-spacing (hash-ref options 'characterSpacing 0))

  ;; calculate the actual rendered width of the string after word and character spacing
  (define rendered-width
    ;; wrap this in delay so it's only calculated if needed
    (delay
      (+ (string-width doc text options)
         (* character-spacing (sub1 (string-length text))))))

  ;; create link annotations if the link option is given
  (when (hash-ref options 'link #f)
    (link doc x y-in (force rendered-width) (current-line-height doc) (hash-ref options 'link)))
  
  ;; create underline or strikethrough line
  (when (or (hash-ref options 'underline #f) (hash-ref options 'strike #f))
    (save doc)
    (unless (hash-ref options 'stroke #f)
      (define fill-color-args ($doc-current-fill-color doc))
      (apply stroke-color doc fill-color-args))
    (define new-line-width (if (< ($doc-current-font-size doc) 10) 0.5 (floor (/ ($doc-current-font-size doc) 10))))
    (line-width doc new-line-width)
    (define d (if (hash-ref options 'underline) 1 2))
    (define line-y (+ y-in (/ (current-line-height doc) d)))
    (when (hash-ref options 'underline)
      (set! line-y (+ line-y (- new-line-width))))
    (move-to doc x line-y)
    (line-to doc (+ x (force rendered-width)) line-y)
    (stroke doc)
    (restore doc))

  ;; flip coordinate system
  (save doc)
  (define page-height ($page-height (page doc)))
  (transform doc 1 0 0 -1 0 page-height)
  (define y (- page-height
               y-in
               (* (/ (get-field ascender ($doc-current-font doc)) 1000)
                  ($doc-current-font-size doc))))

  ;; add current font to page if necessary
  (define current-font-id (get-field id ($doc-current-font doc)))
  (hash-ref! (page-fonts (page doc)) current-font-id  (λ () (send ($doc-current-font doc) make-font-ref)))
  
  (add-content doc "BT") ; begin the text object
  (add-content doc (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))) ; text position
  (add-content doc (format "/~a ~a Tf" current-font-id
                           (numberizer ($doc-current-font-size doc)))) ; font and font size
  (let ([mode (+ (if (hash-ref options 'fill #f) 1 0) (if (hash-ref options 'stroke #f) 1 0))])
    (when (and mode (not (zero? mode)))
      (add-content doc (format "~a Tr" mode))))
  (when (not (zero? character-spacing))
    (add-content doc (format "~a Tc" character-spacing)))

  ;; Add the actual text
  ;; 180321: the first call to this operation is very slow from Quad
  ;; 181126: because `encode` calls `layout`
  (match-define (list encoded-char-strs positions)
    (map list->vector (send ($doc-current-font doc) encode text (hash-ref options 'features #f))))
  
  (define scale (/ ($doc-current-font-size doc) 1000.0))
  (define commands empty)
  
  ;; Adds a segment of text to the TJ command buffer
  (define last-segment 0)
  (define (add-segment cur)
    (when (< last-segment cur)
      (define hex (string-append* (for/list ([str (in-vector encoded-char-strs last-segment cur)]) str)))
      (define posn (vector-ref positions (sub1 cur)))
      (define advance (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
      (set! commands (cons (format "<~a> ~a" hex (numberizer (- advance))) commands)))
    (set! last-segment cur))

  ;; Flushes the current TJ commands to the output stream
  (define (flush idx)
    (add-segment idx)
    (when (positive? (length commands))
      (add-content doc (format "[~a] TJ" (string-join (reverse commands) " ")))
      (set! commands empty)))
  
  (for/fold ([had-offset #f] [x x])
            ([(posn idx) (in-indexed positions)])
    (define having-offset
      (cond
        ;; If we have an x or y offset, we have to break out of the current TJ command
        ;; so we can move the text position.
        [(or (not (zero? (glyph-position-x-offset posn))) (not (zero? (glyph-position-y-offset posn))))
         (flush idx)
         (add-content doc ; Move the text position and flush just the current character
                      (format "1 0 0 1 ~a ~a Tm"
                              (numberizer (+ x (* (glyph-position-x-offset posn) scale)))
                              (numberizer (+ y (* (glyph-position-y-offset posn) scale)))))
         (flush (add1 idx))
         #true]
        [else
         ;; If the last character had an offset, reset the text position
         (when had-offset
           (add-content doc (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))))
         ;; Group segments that don't have any advance adjustments
         (unless (zero? (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
           (add-segment (add1 idx)))
         #false]))
    (values having-offset (+ x (* (glyph-position-x-advance posn) scale))))
      
  (flush (vector-length positions))
  (add-content doc "ET") ; end the text object
  (restore doc)) ; restore flipped coordinate system

(define (line doc str [options (mhasheq)])
  (fragment doc str ($doc-x doc) ($doc-y doc) options)
  ;; 181224 unsuppress size tracking in test mode to preserve test 04
  ;; otherwise we'll be doing our own line measurement
  (when (test-mode) (set-$doc-x! doc (+ ($doc-x doc) (string-width doc str)))))

(define (move-down doc [lines 1] #:factor [factor 1])
  (set-$doc-y! (+ ($doc-y doc) (* factor (current-line-height #t) (+ lines ($doc-line-gap doc)))))
  doc)

(define (move-up doc [lines 1])
  (move-down doc #:factor -1))

(define (string-width doc str [options (mhash)])
  (+ (send ($doc-current-font doc) string-width str ($doc-current-font-size doc) (hash-ref options 'features #f))
     (* (hash-ref options 'characterSpacing 0) (sub1 (string-length str)))))
