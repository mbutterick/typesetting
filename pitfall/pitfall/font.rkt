#lang debug racket/base
(require
  "core.rkt"
  racket/match
  racket/class
  "standard-font.rkt"
  "embedded-font.rkt")
(provide (all-defined-out))

(define (open-pdf-font name id)
  (make-object (if (standard-font-name? name) standard-font% embedded-font%) name id))

(define (current-line-height doc [include-gap #f])
  (send ($doc-current-font doc) line-height ($doc-current-font-size doc) include-gap))

(define (font doc src [size #f])
  ;; check registered fonts if src is a string
  (define cache-key
    (match src
      [(? string?) #:when (hash-has-key? ($doc-registered-fonts doc) src)
                   (define ck src)
                   (set! src (hash-ref (hash-ref ($doc-registered-fonts doc) ck) 'src))
                   ck]
      [(? string?) src]
      [_ #false]))
      
  (when size (font-size doc size))

  (match (hash-ref ($doc-font-families doc) cache-key #f) ; check if the font is already in the PDF
    [(? values val) (set-$doc-current-font! doc val)]
    [_ ; if not, load the font
     (define font-index (add1 (length (hash-keys ($doc-font-families doc)))))
     (define id (string->symbol (format "F~a" font-index)))
     (set-$doc-current-font! doc (open-pdf-font src id))
     ;; check for existing font families with the same name already in the PDF
     (match (hash-ref ($doc-font-families doc) (get-field name ($doc-current-font doc)) #f)
       [(? values font) (set-$doc-current-font! doc font)]
       [_ ;; save the font for reuse later
        (when cache-key (hash-set! ($doc-font-families doc) cache-key ($doc-current-font doc)))
        (hash-set! ($doc-font-families doc) (get-field name ($doc-current-font doc)) ($doc-current-font doc))])])
  doc)

(define (font-size doc size)
  (unless (and (number? size) (not (negative? size)))
    (raise-argument-error 'font-size "non-negative number" size))
  (set-$doc-current-font-size! doc size)
  doc)

(define (font-features doc features [unfeatures null])
  (unless (or (not features) (and (list? features) (andmap bytes? features)))
    (raise-argument-error 'font-features "list of byte strings or #f" features))
  (unless (and (list? unfeatures) (andmap bytes? unfeatures))
    (raise-argument-error 'font-features "list of byte strings" unfeatures))
  (set-$doc-current-font-features! doc
                                   (and features
                                        (sort (for/list ([f (in-list features)]
                                                         #:unless (memv f unfeatures))
                                                f) bytes<?)))
  doc)

(define (register-font doc name src)
  (hash-set! ($doc-registered-fonts doc) name (make-hasheq (list (cons 'src src))))
  doc)
