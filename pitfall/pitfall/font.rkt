#lang debug racket/base
(require
  "core.rkt"
  racket/match
  racket/class
  racket/list
  "reference.rkt"
  "font-standard.rkt"
  "font-embedded.rkt")
(provide (all-defined-out))

(define (make-font-ref f)
  (or (pdf-font-ref f)
      (and (set-pdf-font-ref! f (make-ref)) (pdf-font-ref f))))

(define (embed f)
  (define embed-proc (pdf-font-embed f))
  (embed-proc f))

(define (encode f str [options #f])
  (define encode-proc (pdf-font-encode f))
  (encode-proc f str options))

(define (measure-string f str size [options #f])
  (define measure-proc (pdf-font-measure-string f))
  (measure-proc f str size options))

(define (font-end f)
  (unless (or (pdf-font-embedded f) (not (pdf-font-ref f)))
    (embed f)
    (set-pdf-font-embedded! f #t)))

(define (line-height f size [include-gap #f])
  (define gap (if include-gap (pdf-font-line-gap f) 0))
  (* (/ (+ (pdf-font-ascender f) gap (- (pdf-font-descender f))) 1000.0) size))

(define (open-pdf-font name id)
  ((if (standard-font-name? name) make-standard-font make-embedded-font) name id))

(define (current-line-height doc [include-gap #f])
  (line-height (pdf-current-font doc) (pdf-current-font-size doc) include-gap))

(define (font doc src [size #f])
  ;; check registered fonts if src is a string
  (define cache-key
    (match src
      [(? string?) #:when (hash-has-key? (pdf-registered-fonts doc) src)
                   (define ck src)
                   (set! src (hash-ref (hash-ref (pdf-registered-fonts doc) ck) 'src))
                   ck]
      [(? string?) src]
      [_ #false]))
      
  (when size (font-size doc size))

  (match (hash-ref (pdf-font-families doc) cache-key #f) ; check if the font is already in the PDF
    [(? values val) (set-pdf-current-font! doc val)]
    [_ ; if not, load the font
     (define font-index (add1 (length (hash-keys (pdf-font-families doc)))))
     (define id (string->symbol (format "F~a" font-index)))
     (set-pdf-current-font! doc (open-pdf-font src id))
     ;; check for existing font families with the same name already in the PDF
     (match (hash-ref (pdf-font-families doc) (pdf-font-name (pdf-current-font doc)) #f)
       [(? values font) (set-pdf-current-font! doc font)]
       [_ ;; save the font for reuse later
        (when cache-key (hash-set! (pdf-font-families doc) cache-key (pdf-current-font doc)))
        (hash-set! (pdf-font-families doc) (pdf-font-name (pdf-current-font doc)) (pdf-current-font doc))])])
  doc)

(define (font-size doc size)
  (unless (and (number? size) (not (negative? size)))
    (raise-argument-error 'font-size "non-negative number" size))
  (set-pdf-current-font-size! doc size)
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
  
  (set-pdf-current-font-features!
   doc (net-features (append (pdf-current-font-features doc) new-features)))
  doc)

(define (register-font doc name src)
  (hash-set! (pdf-registered-fonts doc) name (make-hasheq (list (cons 'src src))))
  doc)
