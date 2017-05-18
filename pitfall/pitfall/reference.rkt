#lang pitfall/racket
(require "object.rkt" "zlib.rkt")
(provide PDFReference)

(define-subclass object% (PDFReference document id [data (mhash)])
  (super-new)
  (field [gen 0]
         [i-wanna-be-deflated #f]
         [compress (and (· document compress) (not (hash-ref data 'Filter #f)))]
         [chunks empty]
         [offset #f])

  (as-methods
   write
   _write
   end
   toString))


(define/contract (write this data)
  (any/c . ->m . void?)
  (send this _write data #f void))


(define/contract (_write this chunk-in encoding callback)
  ((or/c string? isBuffer?) (or/c string? #f) procedure? . ->m . any/c)
  (define chunk (if (isBuffer? chunk-in)
                    chunk-in
                    (newBuffer (string-append chunk-in "\n"))))
  (hash-ref! (· this data) 'Length 0)
  (when (· this compress) (set-field! i-wanna-be-deflated this #t))
  (push-end-field! chunks this chunk)
  (hash-update! (· this data) 'Length (curry + (buffer-length chunk)))
  (callback))


(define/contract (end this [chunk #f])
  (() ((or/c any/c #f)) . ->*m . void?)

  (when (· this i-wanna-be-deflated)
    (define deflated-chunk (deflate (apply bytes-append (· this chunks))))
    (set-field! chunks this (list deflated-chunk))
    (hash-set*! (· this data)
                'Filter "FlateDecode"
                'Length (buffer-length deflated-chunk)))
  
  (define this-doc (· this document)) 

  (set-field! offset this (· this-doc _offset))

  (send* this-doc
    [_write (format "~a ~a obj" (· this id) (· this gen))]
    [_write (convert (· this data))])

  (let ([this-chunks (· this chunks)])
    (when (positive? (length this-chunks))
      (send this-doc _write "stream")
      (for ([chunk (in-list this-chunks)])
        (send this-doc _write chunk))
      (send this-doc _write "\nendstream")))

  (send* this-doc
    [_write "endobj"]
    [_refEnd this]))


(define/contract (toString this)
  (->m string?)
  (format "~a ~a R" (· this id) (· this gen)))
