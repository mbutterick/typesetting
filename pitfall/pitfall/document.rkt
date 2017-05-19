#lang pitfall/racket
(require "reference.rkt" "object.rkt" "page.rkt")
(require "vector.rkt" "color.rkt" "fonts.rkt" "text.rkt" "images.rkt")
(provide PDFDocument)

(define mixed% (image-mixin (text-mixin (fonts-mixin (color-mixin (vector-mixin object%))))))

(define-subclass mixed% (PDFDocument [options (mhash)])
  (super-new)

  (compress-streams? (hash-ref options 'compress #t))
  (field [byte-strings empty] ; list of byte chunks to push onto; simulates interface of stream.readable
         [version 1.3] ; PDF version
         [_pageBuffer null]
         [_pageBufferStart 0]
         [_offsets null] ; The PDF object store
         [_waiting 0]
         [_ended #f]
         [_offset 0]
         [_root (ref this
                     (mhash 'Type "Catalog"
                            'Pages (ref this
                                        (mhash 'Type "Pages"
                                               'Count 0
                                               'Kids empty))))] ; top object
         [page #f] ; The current page
         [x 0]
         [y 0]
         [info (mhash
                'Producer "PITKIT"
                'Creator "PITKIT"
                'CreationDate (seconds->date (if (test-mode)
                                                 0
                                                 (current-seconds)) #f))]  ; Initialize the metadata
         [op #f] ; for `pipe`
         [_info #f]) ; for `end`

  ;; Initialize mixins
  (· this initColor)
  (· this initVector)
  (· this initFonts)
  (· this initText)
  ;(· this initImages)

  (as-methods
   addPage
   flushPages
   ref
   push
   _write
   addContent
   _refEnd
   pipe
   end)

  (for ([(key val) (in-hash (hash-ref options 'info (hash)))]) ; if no 'info key, nothing will be copied from (hash)
       (hash-set! info key val))

  ;; Write the header
  (_write this (format "%PDF-~a" version)) ;  PDF version
  (let ([c (integer->char #xFF)])
    (_write this (string-append "%" (string c c c c)))) ; 4 binary chars, as recommended by the spec

  ;; Add the first page
  (unless (not (hash-ref options 'autoFirstPage #t))
    (addPage this)))

    
(define/contract (addPage this [options-arg (· this options)])
  (() (hash?) . ->*m . object?)
  ;; end the current page if needed
  (unless (hash-ref (· this options) 'bufferPages #f)
    (send this flushPages))

  ;; create a page object
  (set-field! page this (make-object PDFPage this options-arg))
  (push-end-field! _pageBuffer this (· this page))
  ;; add the page to the object store
  (define pages (· this _root payload Pages payload))
  (hash-update! pages 'Kids (curry cons (· this page dictionary)))
  (hash-update! pages 'Count add1)

  ;; reset x and y coordinates
  (set-field! x this (· this page margins left))
  (set-field! y this (· this page margins top))
  ;; flip PDF coordinate system so that the origin is in
  ;; the top left rather than the bottom left
  (set-field! _ctm this default-ctm-value)
  (send this transform 1 0 0 -1 0 (· this page height))

  #;(@emit "pageAdded") ; from eventemitter interface
  this)


(define/contract (flushPages this)
  (->m list?)
  ;; this local variable exists so we're future-proof against
  ;; reentrant calls to flushPages.
  (define pages (· this _pageBuffer))
  (set-field! _pageBuffer this empty)
  (increment-field! _pageBufferStart this (length pages))
  (for/list ([p (in-list pages)])
            (· p end)))


;; every js function argument is 'undefined' by default
;; so even a function defined without default values
;; can be called without arguments
(define/contract (ref this [payload (mhash)])
  (() (hash?) . ->*m . (is-a?/c PDFReference))
  (define newref (make-object PDFReference this (add1 (length (· this _offsets))) payload))
  (push-end-field! _offsets this #f) ; placeholder for this object's offset once it is finalized
  (increment-field! _waiting this)
  newref)


(define/contract (push this chunk)
  (isBuffer? . ->m . void?)
  (push-field! byte-strings this chunk))


(define/contract (_write this data)
  (any/c . ->m . void?)
  (let ([data (if (not (isBuffer? data))
                  (newBuffer (string-append data "\n"))
                  data)])
    (push this data)
    (increment-field! _offset this (buffer-length data))
    (void)))


(define/contract (addContent this data)
  (any/c . ->m . object?)
  (send (· this page) write data)
  this)


(define/contract (_refEnd this ref)
  ((is-a?/c PDFReference) . ->m . void?)
  (set-field! _offsets this (for/list ([(offset idx) (in-indexed (· this _offsets))])
                                      (if (= (· ref id) (add1 idx))
                                          (· ref offset)
                                          offset)))
  (increment-field! _waiting this -1)
  (if (and (zero? (· this _waiting)) (· this _ended))
      (· this _finalize)
      (set-field! _ended this #f)))


(define/contract (pipe this port)
  (port? . ->m . void?)
  (set-field! op this port))


(define/contract (end this)
  (->m boolean?)
  (flushPages this)
  (set-field! _info this (ref this))
  (for ([(key val) (in-hash (· this info))])
       ;; upgrade string literal to String struct
       (hash-set! (· this _info payload) key (if (string? val) (String val) val)))
  (· this _info end)

  (for ([font (in-hash-values (· this _fontFamilies))])
       (· font finalize))

  (· this _root end)
  (· this _root payload Pages end)

  (cond
    [(positive? (· this _waiting)) (set-field! _ended this #t)]
    [else
     ;; generate xref
     (define xref-offset (· this _offset))
     (with-method ([this-write (this _write)])
       (define this-offsets (· this _offsets)) 
       (this-write "xref")
       (this-write (format "0 ~a" (add1 (length this-offsets))))
       (this-write "0000000000 65535 f ")
       (for ([offset (in-list this-offsets)])
            (this-write @string-append{@(~r offset #:min-width 10 #:pad-string "0") 00000 n }))
       (this-write "trailer") ;; trailer
       (this-write (convert
                    (mhash 'Size (add1 (length this-offsets))
                           'Root (· this _root)
                           'Info (· this _info))))
       (this-write "startxref")
       (this-write (number xref-offset))
       (this-write "%%EOF"))
     
     ;; end the stream
     ;; in node you (@push null) which signals to the stream
     ;; to copy to its output port
     ;; here we'll do it manually
     (define this-op (· this op))
     (copy-port (open-input-bytes
                 (apply bytes-append (reverse (· this byte-strings)))) this-op)
     (close-output-port this-op)])
  #t)




(module+ test
  (define d (new PDFDocument)))