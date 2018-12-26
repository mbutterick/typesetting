#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/match
  sugar/unstable/dict
  "font-open.rkt")
(provide (all-defined-out))

(define (current-line-height doc [include-gap #f])
  (send ($doc-current-font doc) line-height ($doc-current-font-size doc) include-gap))

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
     (set-$doc-font-count! doc (add1 ($doc-font-count doc)))
     (define id (string->symbol (format "F~a" ($doc-font-count doc))))
     (set-$doc-current-font! doc (PDFFont-open src family id))
     ;; check for existing font families with the same name already in the PDF
     (match (hash-ref ($doc-font-families doc) (get-field name ($doc-current-font doc)) #f)
       [(? values font) (set-$doc-current-font! doc font)]
       [_ ;; save the font for reuse later
        (when cache-key (hash-set! ($doc-font-families doc) cache-key ($doc-current-font doc)))
        (hash-set! ($doc-font-families doc) (get-field name ($doc-current-font doc)) ($doc-current-font doc))])])
  doc)

(define (font-size doc size)
  (set-$doc-current-font-size! doc size)
  doc)

(define (register-font doc name src [family #f])
  (hash-set! ($doc-registered-fonts doc) name (make-hash (list (cons 'src src)
                                                               (cons 'family family))))
  doc)
