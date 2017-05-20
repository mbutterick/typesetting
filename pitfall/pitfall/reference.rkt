#lang pitfall/racket
(require "object.rkt" "zlib.rkt")
(provide PDFReference)

(define-subclass object% (PDFReference document id [payload (mhash)])
  (super-new)
  (field [byte-strings empty]
         [offset #f])

  (as-methods
   write
   end
   toString))


(define/contract (write this x)
  ((or/c string? isBuffer?) . ->m . void?)
  (push-field! byte-strings this (if (isBuffer? x)
                                     x
                                     (bytes-append (newBuffer x) #"\n"))))

(define got-byte-strings? pair?)

(define/contract (end this [chunk #f])
  (() ((or/c string? isBuffer?)) . ->*m . void?)
  (when chunk
    (send this write chunk))

  (report* 'end! (· this id))
  (define bstrs-to-write
    (let ([current-bstrs (reverse (· this byte-strings))])
      (if (and (or (compress-streams?)
               (equal? (hash-ref (· this payload) 'Filter #f) "FlateDecode"))
               (got-byte-strings? current-bstrs))
          (let ([deflated-chunk (deflate (apply bytes-append current-bstrs))])
            (hash-set! (· this payload) 'Filter "FlateDecode")
            (list deflated-chunk))
          current-bstrs)))
  
  (when (got-byte-strings? bstrs-to-write)
    (hash-set! (· this payload) 'Length (apply + (map buffer-length bstrs-to-write))))

  (define this-doc (· this document)) 
  (set-field! offset this (· this-doc _offset))
  
  (with-method ([doc_write (this-doc write)])
    (doc_write (format "~a 0 obj" (· this id)))
    (doc_write (convert (· this payload)))
    (when (got-byte-strings? bstrs-to-write)
      (doc_write "stream")
      (for ([bstr (in-list bstrs-to-write)])
        (doc_write bstr))
      (doc_write "\nendstream"))
    (doc_write "endobj"))

  (report (· this id))
  (send this-doc _refEnd this))


(define/contract (toString this)
  (->m string?)
  (format "~a 0 R" (· this id)))
