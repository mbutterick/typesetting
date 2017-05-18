#lang pitfall/racket
(require "object.rkt" "zlib.rkt")
(provide PDFReference)

#|
(when (· this deflate)
    (define compressed-chunks (map deflate (· this chunks)))
    (set-field! chunks this compressed-chunks)
    (hash-set! (· this data) 'Length (apply + (map bytes-length compressed-chunks))))
|#

(define-subclass object% (PDFReference document id [data (mhash)])
  (super-new)
  (field [gen 0]
         [deflate #f]
         [compress (and (· document compress)
                        (not (hash-ref data 'Filter #f)))]
         [uncompressedLength 0]
         [chunks empty]
         [offset #f])

  (as-methods
   initDeflate
   write
   _write
   end
   finalize
   toString))

(define/contract (initDeflate this)
  (->m void?)
  (hash-ref! (· this data) 'Filter "FlateDecode")
  (set-field! deflate this #t))

(define/contract (write this data)
  (any/c . ->m . void?)
  (send this _write data #f void))

(define/contract (_write this chunk-in encoding callback)
  (any/c (or/c string? #f) procedure? . ->m . any/c)
  (define chunk (if (isBuffer? chunk-in)
                    chunk-in
                    (newBuffer (string-append chunk-in "\n"))))
  (increment-field! uncompressedLength this (buffer-length chunk))
  (hash-ref! (· this data) 'Length 0)

  (when (· this compress) (send this initDeflate))

  (push-end-field! chunks this chunk)
  (hash-update! (· this data) 'Length (curry + (buffer-length chunk)))
  (callback))


(define/contract (end this [chunk #f])
  (() ((or/c any/c #f)) . ->*m . void?)
  ; (super) ; todo
  (when (· this deflate)
    (set-field! chunks this (list (deflate (apply bytes-append (· this chunks)))))
    (hash-set! (· this data) 'Length (apply + (map buffer-length (· this chunks)))))
  #;(report* 'end! (· this id) (· this chunks))
  (send this finalize))


(define/contract (finalize this)
  (->m void?)
  #;(report* 'finalize! (· this id) (· this chunks))

  (set-field! offset this (· this document _offset))

  (define this-doc (· this document)) 
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
