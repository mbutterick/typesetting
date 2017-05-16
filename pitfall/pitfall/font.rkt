#lang pitfall/racket
(require "font/standard-fonts.rkt" "font/afm.rkt" "reference.rkt")
(provide PDFFont PDFFont-open)

(define PDFFont
  (class object%
    (super-new)
    (field [dictionary #f]
           [embedded #f])

    (as-methods
     ref
     finalize)
    ))

(define (PDFFont-open document src family id)
  (cond
    [(string? src)
     (when (isStandardFont src)
       (make-object StandardFont document src id))]))


(define/contract (ref this)
  (->m (is-a?/c PDFReference))
  (unless (· this dictionary)
    (set-field! dictionary this (send (· this document) ref)))
  (· this dictionary))

(define/contract (finalize this)
  (->m void?)
  (unless (or (· this embedded) (not (· this dictionary)))
    (· this embed)
    (set-field! embedded this #t)))


(define StandardFont
  (class PDFFont
    (super-new)
    (init-field document name id)
    (field [font (make-object AFMFont ((hash-ref STANDARD_FONTS name)))])
    (field [ascender (· font ascender)]
           [descender (· font descender)]
           [bbox (· font bbox)]
           [lineGap (· font lineGap)])
    (as-methods
     embed)))

(define/contract (embed this)
  (->m void?)
  (set-field! data (· this dictionary)
              (mhash 'Type "Font"
                     'BaseFont (· this name)
                     'Subtype "Type1"
                     'Encoding "WinAnsiEncoding"))
  (· this dictionary end))



(module+ test
  (define stdfont (make-object StandardFont #f "Helvetica" #f))
  stdfont)