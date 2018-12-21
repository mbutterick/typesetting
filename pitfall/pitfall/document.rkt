#lang at-exp racket/base
(require
  "helper.rkt"
  "param.rkt"
  "struct.rkt"
  racket/class
  racket/format
  racket/contract
  racket/list
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/unstable/port
  "reference.rkt"
  "object.rkt"
  "page.rkt"
  "vector.rkt"
  "color.rkt"
  "fonts.rkt"
  "text.rkt"
  "images.rkt"
  "annotations.rkt")
(provide PDFDocument)

(define mixed% (annotation-mixin (image-mixin (text-mixin (fonts-mixin (color-mixin (vector-mixin object%)))))))

(define pdf-version 1.3)

(define-subclass mixed% (PDFDocument [options (mhash)])
  (compress-streams? (hash-ref options 'compress #t))
  (current-doc-offset 0)
  
  (field [byte-strings empty]
         [_pageBuffer null]
         [_offsets (mhash)] ; The PDF object store
         [_ended #f]
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
         [output-port #f]) ; for `pipe`
        

  ;; Initialize mixins
  (· this initColor)
  (· this initVector)
  (· this initFonts)
  (· this initText)
  (· this initImages)

  (as-methods
   addPage
   flushPages
   ref
   write
   addContent
   _refEnd
   pipe
   end)

  (for ([(key val) (in-hash (hash-ref options 'info (hash)))]) ; if no 'info key, nothing will be copied from (hash)
       (hash-set! info key val))

  ;; Write the header
  (write this (format "%PDF-~a" pdf-version)) ;  PDF version
  (write this (string-append "%" (list->string (map integer->char (make-list 4 #xFF))))) ; 4 binary chars, as recommended by the spec

  ;; Add the first page
  (when (hash-ref options 'autoFirstPage #t) (addPage this)))

    
(define/contract (addPage this [options-arg (· this options)])
  (() (hash?) . ->*m . object?)
  ;; end the current page if needed
  (unless (hash-ref (· this options) 'bufferPages #f)
    (send this flushPages))

  ;; create a page object
  (set-field! page this (make-object PDFPage this options-arg))
  (push-field! _pageBuffer this (· this page))
  
  ;; in Kids, store page dictionaries in correct order
  ;; this determines order in document
  (define pages (· this _root payload Pages payload))
  (hash-update! pages 'Kids (λ (val) (append val (list (· this page dictionary)))))
  (hash-set! pages 'Count (length (hash-ref pages 'Kids)))

  ;; reset x and y coordinates
  (set-field! x this (· this page margins left))
  (set-field! y this (· this page margins top))
  ;; flip PDF coordinate system so that the origin is in
  ;; the top left rather than the bottom left
  (set-field! _ctm this default-ctm-value)
  (send this transform 1 0 0 -1 0 (· this page height))
  this)


(define/contract (flushPages this)
  (->m void?)
  (define pb (· this _pageBuffer))
  (for-each (λ (p) (· p end)) pb)
  (set-field! _pageBuffer this empty))


;; every js function argument is 'undefined' by default
;; so even a function defined without default values
;; can be called without arguments
(define/contract (ref this [payload (mhash)])
  (() (hash?) . ->*m . (is-a?/c PDFReference))
  (define next-refid (add1 (length (hash-keys (· this _offsets)))))
  (hash-set! (· this _offsets) next-refid 'missing-ref-offset)
  (make-object PDFReference this next-refid payload))


(define/contract (write this x)
  ((or/c string? isBuffer?) . ->m . any/c)
  (define bstr (if (not (isBuffer? x))
                   (newBuffer (string-append x "\n"))
                   x))
  (push-field! byte-strings this bstr)
  (current-doc-offset (+ (current-doc-offset) (buffer-length bstr))))


(define/contract (addContent this data)
  (any/c . ->m . object?)
  (send (· this page) write data)
  this)


(define/contract (offsets-missing? this)
  (->m boolean?)
  (positive? (length (filter (λ (v) (eq? 'missing-ref-offset v)) (hash-values (· this _offsets))))))


(define/contract (_refEnd this ref)
  ((is-a?/c PDFReference) . ->m . void?)
  #;(report* (· ref id) (· this _offsets))
  (hash-set! (· this _offsets) (· ref id) (· ref offset)))


(define/contract (pipe this port)
  (port? . ->m . void?)
  (set-field! output-port this port))


(define/contract (end this) ; called from source file to finish doc
  (->m void?)
  #;(report* 'start-end)
  #;(report* (· this _offsets))

  (flushPages this)
  (define _info (ref this))
  (for ([(key val) (in-hash (· this info))])
       ;; upgrade string literal to String struct
       (hash-set! (· _info payload) key (if (string? val) (String val) val)))


  #;(report* (· this _offsets))
  (· _info end)

  (for ([font (in-hash-values (· this _fontFamilies))])
       (· font finalize))

  #;(report* (· this _offsets))

  (· this _root end)
  #;(report* (· this _offsets))
  (· this _root payload Pages end)
  #;(report* (· this _offsets))

  ;; generate xref
  (define xref-offset (current-doc-offset))
  (with-method ([this-write (this write)])
    (define sorted-offset-records  (sort (hash->list (· this _offsets)) < #:key car)) ; sort by refid
    (define this-offsets (map cdr sorted-offset-records))
    (define this-idxs (map car sorted-offset-records))
    (this-write "xref")
    (this-write (format "0 ~a" (add1 (length this-offsets))))
    (this-write "0000000000 65535 f ")
    (let ([missing-offsets (for/list ([offset (in-list this-offsets)]
                                      [idx (in-list this-idxs)]
                                      #:unless (number? offset))
                                     idx)])
      (unless (empty? missing-offsets)
        (raise-argument-error 'document:end "numerical offsets" missing-offsets)))
    (for ([offset (in-list this-offsets)]
          [idx (in-list this-idxs)])
         (this-write @string-append{@(~r offset #:min-width 10 #:pad-string "0") 00000 n }))
    (this-write "trailer") ;; trailer
    (this-write (convert
                 (mhash 'Size (add1 (length this-offsets))
                        'Root (· this _root)
                        'Info _info)))
    (this-write "startxref")
    (this-write (number xref-offset))
    (this-write "%%EOF"))
     
  ;; end the stream
  ;; in node you (@push null) which signals to the stream
  ;; to copy to its output port
  ;; here we'll do it manually
  (define this-output-port (· this output-port))
  (copy-port (open-input-bytes
              (apply bytes-append (reverse (· this byte-strings)))) this-output-port)
  (close-output-port this-output-port))




(module+ test
  (define d (new PDFDocument)))