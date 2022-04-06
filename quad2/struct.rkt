#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(struct $drawing-inst () #:transparent)
(struct $move $drawing-inst (posn) #:transparent) ; an absolute location in coordinate system (not relative to last loc)
(struct $text $drawing-inst (charint) #:transparent)
(struct $doc $drawing-inst (inst) #:transparent)
(struct $page $drawing-inst (inst) #:transparent)

(struct attr-key (name) #:transparent)

(define-syntax (define-attr-key-types stx)
  (syntax-case stx ()
    [(_ ID ...)
     (with-syntax ([(ATTR-ID-KEY ...) (map (λ (id-stx) (format-id stx "attr-~a-key" id-stx)) (syntax->list #'(ID ...)))])
       #'(begin
           (struct ATTR-ID-KEY attr-key () #:transparent) ...))]))

;; for type X, creates struct called attr-X-key
(define-attr-key-types
  uncased-string
  cased-string
  dimension-string
  path
  numeric
  boolean
  ignored)