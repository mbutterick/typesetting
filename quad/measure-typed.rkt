#lang typed/racket/base
(require (for-syntax typed/racket/base))
(require typed/racket/class math/flonum racket/list racket/file)
(require/typed racket/draw 
               [record-dc%  (Class (init-field) 
                                   (get-text-extent (String (Instance (Class (init-field))) Any . -> . (values Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real))))]
               [make-font ((#:size Nonnegative-Float) (#:style Symbol) (#:weight Symbol) (#:face String) . -> . (Instance (Class (init-field))))])
(require/typed racket/serialize [serialize (Any . -> . Any)]
               [deserialize (Any . -> . (HashTable Any Any))])
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file)

(define precision 4.0)
(define base (expt 10.0 precision))
(define max-size 1024.0)
(define dc (new record-dc%))
(define-type Measurement-Result-Type (List Float Float Float Float))
(define mrt? (make-predicate Measurement-Result-Type))
(define-type MMS-Type ((String String) (Symbol Symbol) . ->* . Measurement-Result-Type))
(define current-text-cache (make-parameter ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type) '())))
(define current-text-cache-changed? : (Parameterof Boolean) (make-parameter #f))
(define current-font-cache (make-parameter ((inst make-hash (List String Symbol Symbol) (Instance (Class (init-field)))) '())))


(: round-float (Float . -> . Float))
(define (round-float x)
  (/ (round (* base x)) base))

(: get-cache-file-path (-> Path))
(define (get-cache-file-path) 
  (build-path "font.cache"))


(: update-text-cache-file (-> Void))
(define (update-text-cache-file)
  (when (current-text-cache-changed?)
    (write-to-file (serialize (current-text-cache)) (get-cache-file-path) #:exists 'replace)
    (current-text-cache-changed? #f)))


(:  load-text-cache-file (-> Void))
(define (load-text-cache-file) 
  (define cache-file-path (get-cache-file-path))
  (current-text-cache (if (file-exists? cache-file-path)
                          (cast (deserialize (file->value cache-file-path)) (HashTable (List String String Symbol Symbol) Measurement-Result-Type))
                          ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type) '()))))


(: get-cached-font (String Symbol Symbol . -> . (Instance (Class (init-field)))))
(define (get-cached-font font weight style)
  (hash-ref! (current-font-cache) (list font weight style) (λ() (make-font #:size max-size #:style style #:weight weight #:face font))))


(: measure-max-size-base (String String Symbol Symbol . -> . Measurement-Result-Type))
(define (measure-max-size-base text font weight style)
  (: hash-updater (-> Measurement-Result-Type))
  (define (hash-updater) 
    (current-text-cache-changed? #t)
    (define font-instance (get-cached-font font weight style))
    ;; 'combine' boolean only makes a difference for two or more chars, so use (>= (string-length text) 1) for speed
    (define-values (width height descent extra) (send dc get-text-extent text font-instance (>= (string-length text) 1)))
    ;; avoid `map` here because it requires a cast to ensure the type
    ;; this seems like a bug in TR: doesn't recognize (List Float Float Float Float) as subtype of (Listof Float)
    (list (fl width) (fl height) (fl descent) (fl extra)))
  ((inst hash-ref! (List String String Symbol Symbol) Measurement-Result-Type) (current-text-cache) (list text font weight style) hash-updater))


;; rather than use define/caching from the (untyped) sugar/cache, implement the cache "manually"
(define measure-cache ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type)))
(: measure-max-size MMS-Type)
(define (measure-max-size text font [weight 'normal] [style 'normal])
  (hash-ref! measure-cache (list text font weight style) (λ () (measure-max-size-base text font weight style))))


(define-syntax-rule (width x) (first x))
(define-syntax-rule (height x) (second x))
(define-syntax-rule (descent x) (third x))
#;(define-syntax-rule (extra x) (fourth x)) 


;; works by taking max size and scaling it down. Allows caching of results.
(: measure-text ((String Positive-Float String) (Symbol Symbol) . ->* . Float))
(define (measure-text text size font [weight 'normal] [style 'normal])
  (define raw-width (width (measure-max-size text font weight style)))
  (round-float (/ (* raw-width size) max-size)))


;; works by taking max size and scaling it down. Allows caching of results.
(: measure-ascent ((String Positive-Float String) (Symbol Symbol) . ->* . Float))
(define (measure-ascent text size font [weight 'normal] [style 'normal])
  (define result-list : Measurement-Result-Type (measure-max-size text font weight style))
  (define raw-baseline-distance (- (height result-list) (descent result-list)))
  (round-float (/ (* raw-baseline-distance size) max-size)))