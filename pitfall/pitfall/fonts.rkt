#lang debug racket/base
(require
  racket/class
  racket/match
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  "font-open.rkt")
(provide fonts-mixin)

(define (fonts-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [@font-families (mhash)]
           [@font-count 0]
           [(@current-font-size current-font-size) 12] ; font state used by text.rkt
           [(@current-font current-font) #f] ; font state used by text.rkt
           [@registered-fonts (mhash)])
    
    (define/public (font src [size-or-family #f] [maybe-size #f])
      (match-define (list family size) (match size-or-family
                                         [(? number?)(list #f size-or-family)]
                                         [_ (list size-or-family maybe-size)]))
      ;; check registered fonts if src is a string
      (define cache-key (match src
                          [(? string?) #:when (hash-has-key? @registered-fonts src)
                                       (define ck src)
                                       (set! src (· (hash-ref @registered-fonts ck) src))
                                       (set! family (· (hash-ref @registered-fonts ck) family))
                                       ck]
                          [_ (match (or family src)
                               [(? string? str) str]
                               [_ #false])]))

      (when size (font-size size))
      (cond       ;; fast path: check if the font is already in the PDF
        [(hash-ref @font-families cache-key #f)
         => (λ (val) (set! @current-font val))]
        [else ;; load the font
         (set! @font-count (add1 @font-count))
         (define id (format "F~a" @font-count))
         (set! @current-font (PDFFont-open this src family id))
         ;; check for existing font familes with the same name already in the PDF
         ;; useful if the font was passed as a buffer
         (match (hash-ref @font-families (· @current-font name) #f)
           [(? values font) (set! @current-font font)]
           [_ ;; save the font for reuse later
            (when cache-key (hash-set! @font-families cache-key @current-font))
            (hash-set! @font-families (· @current-font name) @current-font)])])
      this)

    (define/public (font-size size)
      (set! @current-font-size size)
      this)

    (define/public (current-line-height [includeGap #f])
      (send @current-font lineHeight @current-font-size includeGap))


    (define/public (register-font name src [family #f])
      (hash-set! @registered-fonts name
                 (mhash 'src src 'family family))
      this)))


(module+ test
  (define fo (new (fonts-mixin))))
