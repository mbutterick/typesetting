#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct)
(provide (all-defined-out))

(struct $drawing-inst () #:transparent)
(struct $move $drawing-inst (posn) #:transparent) ; an absolute location in coordinate system (not relative to last loc)
(struct $text $drawing-inst (charint) #:transparent)
(struct $font $drawing-inst (path-string) #:transparent)
(struct $doc-start $drawing-inst () #:transparent)
(struct $doc-end $drawing-inst () #:transparent)
(struct $page-start $drawing-inst (x y) #:transparent)
(struct $page-end $drawing-inst () #:transparent)

(struct attr-key (name mandatory? default) #:transparent
  #:methods gen:custom-write
  [(define (write-proc val out mode)
     (fprintf out (format ":~a" (attr-key-name val))))])

(define (make-attr-key name [mandatory? #false] [default #false])
  (attr-key name mandatory? default))

(define-syntax (define-attr-key-types stx)
  (syntax-case stx ()
    [(_ ID ...)
     (with-syntax ([(ATTR-ID-KEY ...) (map (λ (id-stx) (format-id stx "attr-~a-key" id-stx)) (syntax->list #'(ID ...)))]
                   [(MAKE-ATTR-ID-KEY ...) (map (λ (id-stx) (format-id stx "make-attr-~a-key" id-stx)) (syntax->list #'(ID ...)))])
       #'(begin
           (begin
             (struct ATTR-ID-KEY attr-key () #:transparent)
             (define (MAKE-ATTR-ID-KEY name [mandatory #false] [default #false])
               (ATTR-ID-KEY name mandatory default))) ...))]))

;; for type X, creates struct called attr-X-key
(define-attr-key-types
  uncased-string
  cased-string
  dimension-string
  path
  numeric
  boolean
  set
  unknown)