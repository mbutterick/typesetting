#lang pitfall/racket
(require "object.rkt" "zlib.rkt")
(provide PDFReference)

(define-subclass object% (PDFReference document id [data (mhash)])
  (super-new)
  (field [gen 0]
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
  (push-end-field! chunks this chunk)
  (hash-update! (· this data) 'Length (curry + (buffer-length chunk)) 0)
  (callback))


(define/contract (end this)
  (->m void?)

  (define chunks-to-write
    (let ([current-chunks (· this chunks)])
      (if (and (· this compress) (pair? current-chunks))
          (let ([deflated-chunk (deflate (apply bytes-append current-chunks))])
            (hash-set*! (· this data)
                        'Filter "FlateDecode"
                        'Length (buffer-length deflated-chunk))
            (list deflated-chunk))
          current-chunks)))

  (define this-doc (· this document)) 
  (set-field! offset this (· this-doc _offset))
  
  (with-method ([doc_write (this-doc _write)])
    (doc_write (format "~a ~a obj" (· this id) (· this gen)))
    (doc_write (convert (· this data)))
    (when (pair? chunks-to-write)
      (doc_write "stream")
      (for ([chunk (in-list chunks-to-write)])
           (doc_write chunk))
      (doc_write "\nendstream"))
    (doc_write "endobj"))
  
  (send this-doc _refEnd this))


(define/contract (toString this)
  (->m string?)
  (format "~a ~a R" (· this id) (· this gen)))
