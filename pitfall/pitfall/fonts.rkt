#lang debug racket/base
(require
  "core.rkt"
  "reference.rkt"
  racket/match
  sugar/unstable/dict
  "standard-font.rkt"
  fontland
  "embedded-font.rkt")

(provide (all-defined-out))

(define (make-font-ref font)
  (unless ($font-dictionary font)
    (set-$font-dictionary! font (make-ref)))
  ($font-dictionary font))

(define (font-end font)
  (unless (or ($font-embedded font) (not ($font-dictionary font)))
    (($font-embed-proc font))
    (set-$font-embedded! font #t)))

(define (line-height font size [include-gap #f])
  (define gap (if include-gap ($font-line-gap font) 0))
  (* (/ (+ ($font-ascender font) gap (- ($font-descender font))) 1000.0) size))

(define (PDFFont-open src family id)
  (cond
    [(and (string? src) (standard-font? src)) (make-standard-font src id)]
    [else
     (define font
       (cond
         [(string? src) (open-font src)]
         [(path? src) (open-font (path->string src))]
         ;; todo: other font-loading cases
         [else (raise-argument-error 'PDFFont-open "loadable font thingy" src)]))
     (make-embedded-font font id)]))


(define (current-line-height doc [include-gap #f])
  (line-height ($doc-current-font doc) ($doc-current-font-size doc) include-gap))

(define (font doc src [size-or-family #f] [maybe-size #f])
  (match-define (list family size)
    (match size-or-family
      [(? number?) (list #f size-or-family)]
      [_ (list size-or-family maybe-size)]))
  ;; check registered fonts if src is a string
  (define cache-key
    (match src
      [(? string?) #:when (hash-has-key? ($doc-registered-fonts doc) src)
                   (define ck src)
                   (set! src (hash-ref (hash-ref ($doc-registered-fonts doc) ck) 'src))
                   (set! family (hash-ref (hash-ref ($doc-registered-fonts doc) ck) 'family))
                   ck]
      [_ (match (or family src)
           [(? string? str) str]
           [_ #false])]))
      
  (when size (font-size doc size))

  (match (hash-ref ($doc-font-families doc) cache-key #f) ; check if the font is already in the PDF
    [(? values val) (set-$doc-current-font! doc val)]
    [_ ; if not, load the font
     (define font-index (add1 (length (hash-keys ($doc-font-families doc)))))
     (define id (string->symbol (format "F~a" font-index)))
     (set-$doc-current-font! doc (PDFFont-open src family id))
     ;; check for existing font families with the same name already in the PDF
     (match (hash-ref ($doc-font-families doc) ($font-name ($doc-current-font doc)) #f)
       [(? values font) (set-$doc-current-font! doc font)]
       [_ ;; save the font for reuse later
        (when cache-key (hash-set! ($doc-font-families doc) cache-key ($doc-current-font doc)))
        (hash-set! ($doc-font-families doc) ($font-name ($doc-current-font doc)) ($doc-current-font doc))])])
  doc)

(define (font-size doc size)
  (set-$doc-current-font-size! doc size)
  doc)

(define (register-font doc name src [family #f])
  (hash-set! ($doc-registered-fonts doc) name (make-hash (list (cons 'src src)
                                                               (cons 'family family))))
  doc)
