#lang typed/racket/base
(require (for-syntax typed/racket/base ))
(require typed/racket/class math/flonum racket/list racket/file)
(require/typed racket/draw 
               [record-dc%  (Class (init-field) 
                                   (get-text-extent (String (Instance (Class (init-field))) Any . -> . (values Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real))))]
               [make-font ((#:size Nonnegative-Float) (#:style Symbol) (#:weight Symbol) (#:face String) . -> . (Instance (Class (init-field))))])
(require/typed sugar/cache [make-caching-proc (Procedure . -> . Procedure)])
(require/typed racket/serialize [serialize (Any . -> . Any)]
               [deserialize (Any . -> . (HashTable Any Any))])
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file make-font/caching)

(define precision 4.0)
(define base (flexpt 10.0 precision))

(define-syntax-rule (round-float x)
  (fl/ (flround (fl* base (fl x))) base))

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))

(define dc (new record-dc%))

(define max-size 1024.0) ; use fixnum to trigger faster bitshift division

;; changing max-size invalidates font cache (because it's based on max size, duh)


(define make-font/caching
  (make-caching-proc (λ (font weight style)
                       (make-font #:size max-size #:style style #:weight weight #:face font))))


(define (get-cache-file-path)
  (build-path "font.cache"))


(define current-text-cache (make-parameter (make-hash '())))
(define current-text-cache-changed? (make-parameter #f))
(define current-font-cache (make-parameter (make-hash '())))


(define (update-text-cache-file)
  (when (current-text-cache-changed?)
    (write-to-file (serialize (current-text-cache)) (get-cache-file-path) #:exists 'replace)
    (current-text-cache-changed? #f)))

(define (load-text-cache-file) 
  (define cache-file-path (get-cache-file-path))
  (current-text-cache (if (file-exists? cache-file-path)
                          (deserialize (file->value cache-file-path))
                          (make-hash '()))))


(define-type Measurement-Result-Type (List Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real))
(define-type MMS-Type ((String String) (Symbol Symbol) . ->* . Measurement-Result-Type))

(: measure-max-size-base MMS-Type)
(define (measure-max-size-base text font [weight 'normal] [style 'normal])
  (define font-instance (cast (hash-ref! (current-font-cache) (list font weight style) (λ() (make-font #:size max-size #:style style #:weight weight #:face font))) (Instance (Class (init-field)))))
  ;; 'combine' boolean only makes a difference for two or more chars
  (send dc get-text-extent text font-instance (>= (string-length text) 1))
  (cast (hash-ref! (current-text-cache) (list text font weight style) (λ() #;(current-text-cache-changed? #t)
                                                                        (values->list (send dc get-text-extent text font-instance (>= (string-length text) 1))))) Measurement-Result-Type))

(define measure-cache ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type)))

(: measure-max-size MMS-Type)
(define (measure-max-size text font [weight 'normal] [style 'normal])
  (hash-ref! measure-cache (list text font weight style) (λ () (measure-max-size-base text font weight style))))




(define-syntax-rule (width x) (first x))
(define-syntax-rule (height x) (second x))
(define-syntax-rule (descent x) (third x))
(define-syntax-rule (extra x) (fourth x)) 


(define-syntax-rule (measure-text-max-size text font weight style)
  (width (measure-max-size text font weight style)))

(: measure-text ((String Nonnegative-Float String) (Symbol Symbol) . ->* . Nonnegative-Float))
(define (measure-text text size font [weight 'normal] [style 'normal])
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-measure (measure-text-max-size text font weight style))
  (cast (round-float (/ (* (exact->inexact raw-measure) (exact->inexact size)) max-size)) Nonnegative-Float))


(define-syntax-rule (measure-ascent-max-size text font weight style)
  (let ([result-list (measure-max-size text font weight style)])
    (- (height result-list) (descent result-list))))

(: measure-ascent ((String Nonnegative-Float String) (Symbol Symbol) . ->* . Nonnegative-Float))
(define (measure-ascent text size font [weight 'normal] [style 'normal])
  ;  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-baseline-distance (measure-ascent-max-size text font weight style))
  (cast (round-float (/ (* (exact->inexact raw-baseline-distance) (exact->inexact size)) max-size)) Nonnegative-Float))