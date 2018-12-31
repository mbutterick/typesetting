#lang debug racket/base
(require
  "core.rkt"
  racket/match
  racket/class
  racket/list
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

(define (net-features feats)
  ;; filter out pairs of features with opposing (0 and 1) values
  (let loop ([feats (remove-duplicates feats)]
             [acc null])
    (cond
      [(empty? feats) acc]
      [(empty? (cdr feats)) (loop empty (cons (car feats) acc))]
      [else (define first-feat (car feats))
            (match (cdr feats)
              [(list head ... (? (λ (f) (bytes=? (car f) (car first-feat)))) tail ...)
               (loop (append head tail) acc)]
              [rest (loop rest (cons first-feat acc))])])))

(define (font-features doc [features-on null] [features-off null])
  (unless (and (list? features-on) (andmap bytes? features-on))
    (raise-argument-error 'font-features "list of feature byte strings" features-on))
  (unless (and (list? features-off) (andmap bytes? features-off))
    (raise-argument-error 'font-features "list of feature byte strings" 'features-off))

  (define (make-feat-pairs feats val)
    (for/list ([f (in-list feats)])
      (match f
        [(cons (? bytes?) (? exact-nonnegative-integer?)) f]
        [(? bytes?) (cons f val)]
        [else
         (raise-argument-error 'font-features
                               "byte string or byte string + integer pair" f)])))
  
  (define new-features (append (make-feat-pairs features-on 1)
                               (make-feat-pairs features-off 0)))
  
  (set-$doc-current-font-features!
   doc (net-features (append ($doc-current-font-features doc) new-features)))
  doc)

(define (register-font doc name src)
  (hash-set! ($doc-registered-fonts doc) name (make-hasheq (list (cons 'src src))))
  doc)
