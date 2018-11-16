#lang racket/base
(require "racket.rkt")

(require "reference.rkt")
(provide PDFFont)

(define PDFFont
  (class object%
    (super-new)
    (field [dictionary #f]
           [embedded #f])

    (as-methods
     ref
     finalize
     lineHeight)))


(define/contract (ref this)
  (->m (is-a?/c PDFReference))
  (unless (· this dictionary)
    (set-field! dictionary this (send (· this document) ref)))
  (· this dictionary))


(define/contract (finalize this)
  (->m void?)
  (unless (or (· this embedded) (not (· this dictionary)))
    (send this embed)
    (set-field! embedded this #t)))


(define/contract (lineHeight this size [includeGap #f])
  ((number?)(boolean?) . ->*m . number?)
  (define gap (if includeGap (· this lineGap) 0))
  (* (/ (+ (· this ascender) gap (- (· this descender))) 1000.0) size))


