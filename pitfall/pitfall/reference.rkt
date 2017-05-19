#lang pitfall/racket
(require "object.rkt" "zlib.rkt")
(provide PDFReference)

(define-subclass object% (PDFReference document id [data (mhash)])
  (super-new)
  (field [chunks empty]
         [offset #f])

  (as-methods
   write
   end
   toString))


(define/contract (write this x)
  ((or/c string? isBuffer?) . ->m . void?)
  (push-end-field! chunks this (if (isBuffer? x)
                                   x
                                   (bytes-append (newBuffer x) #"\n"))))

(define got-chunks? pair?)

(define/contract (end this)
  (->m void?)

  (define chunks-to-write
    (let ([current-chunks (· this chunks)])
      (if (and (compress-streams?)
               (not (hash-ref (· this data) 'Filter #f))
               (got-chunks? current-chunks))
          (let ([deflated-chunk (deflate (apply bytes-append current-chunks))])
            (hash-set! (· this data) 'Filter "FlateDecode")
            (list deflated-chunk))
          current-chunks)))
  
  (when (got-chunks? chunks-to-write)
    (hash-set! (· this data) 'Length (apply + (map buffer-length chunks-to-write))))

  (define this-doc (· this document)) 
  (set-field! offset this (· this-doc _offset))
  
  (with-method ([doc_write (this-doc _write)])
    (doc_write (format "~a 0 obj" (· this id)))
    (doc_write (convert (· this data)))
    (when (got-chunks? chunks-to-write)
      (doc_write "stream")
      (for ([chunk (in-list chunks-to-write)])
           (doc_write chunk))
      (doc_write "\nendstream"))
    (doc_write "endobj"))
  
  (send this-doc _refEnd this))


(define/contract (toString this)
  (->m string?)
  (format "~a 0 R" (· this id)))
