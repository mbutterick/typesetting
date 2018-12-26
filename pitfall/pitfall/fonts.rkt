#lang debug racket/base
(require
  racket/class
  racket/match
  sugar/unstable/dict
  "font-open.rkt")
(provide fonts-mixin)

(define (fonts-mixin [% object%])
  (class %
    (super-new)
    (field [@font-families (make-hash)]
           [@font-count 0]
           [(@current-font-size current-font-size) 12] ; font state used by text.rkt
           [(@current-font current-font) #f] ; font state used by text.rkt
           [@registered-fonts (make-hash)])
    
    (define/public (font src [size-or-family #f] [maybe-size #f])
      (match-define (list family size)
        (match size-or-family
          [(? number?) (list #f size-or-family)]
          [_ (list size-or-family maybe-size)]))
      ;; check registered fonts if src is a string
      (define cache-key
        (match src
          [(? string?) #:when (hash-has-key? @registered-fonts src)
                       (define ck src)
                       (set! src (hash-ref (hash-ref @registered-fonts ck) 'src))
                       (set! family (hash-ref (hash-ref @registered-fonts ck) 'family))
                       ck]
          [_ (match (or family src)
               [(? string? str) str]
               [_ #false])]))
      
      (when size (font-size size))

      (match (hash-ref @font-families cache-key #f) ; check if the font is already in the PDF
        [(? values val) (set! @current-font val)]
        [_ ; if not, load the font
         (set! @font-count (add1 @font-count))
         (define id (string->symbol (format "F~a" @font-count)))
         (set! @current-font (PDFFont-open src family id))
         ;; check for existing font families with the same name already in the PDF
         (match (hash-ref @font-families (get-field name @current-font) #f)
           [(? values font) (set! @current-font font)]
           [_ ;; save the font for reuse later
            (when cache-key (hash-set! @font-families cache-key @current-font))
            (hash-set! @font-families (get-field name @current-font) @current-font)])])
      this)

    (define/public (font-size size)
      (set! @current-font-size size)
      this)

    (define/public (current-line-height [include-gap #f])
      (send @current-font line-height @current-font-size include-gap))

    (define/public (register-font name src [family #f])
      (hash-set! @registered-fonts name (make-hash (list (cons 'src src)
                                                         (cons 'family family))))
      this)))


(module+ test
  (define fo (new (fonts-mixin))))
