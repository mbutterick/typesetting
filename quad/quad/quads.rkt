#lang racket/base
(provide (all-defined-out))
(require (for-syntax racket/string racket/base racket/syntax))

(struct $quad (attrs dim val) #:transparent #:mutable)
(struct $black $quad () #:transparent)
(struct $white $quad () #:transparent)
(struct $skip $quad () #:transparent)
(struct $shim $quad () #:transparent)

(define quad? $quad?)

(define quad-attrs $quad-attrs)
(define quad-val $quad-val)

(define (quad-attrs? x) (list? x))


#|
Attrs needed to specify rendered appearance:
(font) family
(font) style 
(font) size
color
background
position
measure (line width)

|#
(define default-attrs (hasheq 'size 10 'font "sc.otf")) ; Source Code Pro, 12 pt, chars are 6pt wide
(define (quad attr . xs)
  ($quad (or attr (make-attrs)) 0 xs))

(struct $attrs (size font) #:transparent)
(define (make-attrs #:size [size #f]
                    #:font [font #f])
  (hasheq 'size size 'font font))


(define (quad-dim q)
  ($quad-dim q))

(define (quad-font q)
  (hash-ref (quad-attrs q) 'font))
(define (quad-font-size q)
  (hash-ref (quad-attrs q) 'size))

(define (quad-dim-set! q val)
  (set-$quad-dim! q val))

(define (override-with dest source)
  ;; replace missing values in dest with values from source
  (for/hasheq ([k (in-hash-keys source)])
              (values k (or (hash-ref dest k) (hash-ref source k)))))

(require (for-syntax sugar/debug))
(define-syntax (define-break stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([BREAK-NAME (string->symbol (string-upcase (symbol->string (syntax->datum #'name))))])
       #'(define (name) (quad #f 'BREAK-NAME)))]))

(define-break page-break)
(define-break column-break)
(define-break block-break)
(define-break line-break)


(module+ test
  (require rackunit)
  (define q (quad #f "bar"))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-attrs q) (make-attrs))
  (check-equal? (quad-val q) '("bar")))