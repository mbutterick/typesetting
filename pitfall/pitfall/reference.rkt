#lang pitfall/racket
(require "object.rkt")
(provide PDFReference)

(define PDFReference
  (class object%
    (super-new)
    (init-field document id [data (mhash)])
    (field [gen 0]
           [deflate #f]
           [compress (and (compression-enabled)
                          (· document compress)
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
     toString)))

(define/contract (initDeflate this)
  (->m void?)
  (hash-ref! (· this data) 'Filter "FlateDecode"))

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
  (cond
    #;[(· this compress) (when (not (· this deflate)) (initDeflate))
                         (send deflater write chunk)] ; todo: implement compression
    [else (push-end-field! chunks this chunk)
          (hash-update! (· this data) 'Length (λ (len) (+ len (buffer-length chunk))))])
  (callback))

(define/contract (end this [chunk #f])
  (() ((or/c any/c #f)) . ->*m . void?)
  ; (super) ; todo
  (if (· this deflate)
      (void) ; todo (deflate-end)
      (send this finalize)))

(define/contract (finalize this)
  (->m void?)
  (set-field! offset this (· this document _offset))
      
  (send (· this document) _write (format "~a ~a obj" (· this id) (· this gen)))
  (send (· this document) _write (convert (· this data)))

  (when (positive? (length (· this chunks)))
    (send (· this document) _write "stream")
    (for ([chunk (in-list (· this chunks))])
      (send (· this document) _write chunk))

    (set-field! chunks this null) ; free up memory
    (send (· this document) _write "\nendstream"))

  (send (· this document) _write "endobj")
  (send (· this document) _refEnd this))

(define/contract (toString this)
  (->m string?)
  (format "~a ~a R" (· this id) (· this gen)))
