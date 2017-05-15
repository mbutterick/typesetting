#lang pitfall/racket
(require "reference.rkt" "object.rkt" "page.rkt" "vector.rkt" "color.rkt")
(provide PDFDocument)

(define mixed% (color-mixin (vector-mixin object%)))

(define PDFDocument
  (class mixed% ; actually is an instance of readable.Stream, which is an input port
    (init-field [options (mhash)])
    (super-new)

    (field [byte-strings empty] ; list of byte chunks to push onto; simulates interface of stream.readable
           [version 1.3] ; PDF version
           [compress (hash-ref options 'compress #t)] ; Whether streams should be compressed
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
                  'Producer "PitfallKit"
                  'Creator "PitfallKit"
                  'CreationDate (seconds->date (if (test-mode)
                                                   0
                                                   (current-seconds)) #f))]  ; Initialize the metadata
           [op #f] ; for `pipe`
           [_info #f]) ; for `end`

    ;; Initialize mixins
    (· this initColor)
    (· this initVector)
    #;(· this initFonts) ; todo
    #;(· this initText) ; todo
    #;(· this initImages) ; todo

    (as-methods
     addPage
     flushPages
     ref
     push
     _write
     addContent
     _refEnd
     pipe
     end
     _finalize)

    (for ([(key val) (in-hash (hash-ref options 'info (hash)))]) ; if no 'info key, nothing will be copied from (hash)
      (hash-set! info key val))

    ;; Write the header
    (_write this (format "%PDF-~a" version)) ;  PDF version
    (let ([c (integer->char #xFF)])
      (_write this (string-append "%" (string c c c c)))) ; 4 binary chars, as recommended by the spec

    ;; Add the first page
    (unless (not (hash-ref options 'autoFirstPage #t))
      (addPage this))))

    
(define/contract (addPage this [options-arg (· this options)])
  (() (hash?) . ->*m . object?)
  ;; end the current page if needed
  (unless (hash-ref (· this options) 'bufferPages #f)
    (send this flushPages))

  ;; create a page object
  (set-field! page this (make-object PDFPage this options-arg))
  (push-end-field! _pageBuffer this (· this page))
  ;; add the page to the object store
  (define pages (· this _root data Pages data))
  (hash-update! pages 'Kids (curry cons (· this page dictionary)) null)
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
  (·map end pages))


;; every js function argument is 'undefined' by default
;; so even a function defined without default values
;; can be called without arguments
(define/contract (ref this [data (mhash)])
  (() (hash?) . ->*m . (is-a?/c PDFReference))
  (define newref (make-object PDFReference this (add1 (length (· this _offsets))) data))
  (push-end-field! _offsets this #f) ; placeholder for this object's offset once it is finalized
  (increment-field! _waiting this)
  newref)


(define/contract (push this chunk)
  (any/c . ->m . void?)
  (push-field! byte-strings this chunk))


(define/contract (_write this data)
  (any/c . ->m . void?)
  (let ([data (if (not (isBuffer? data))
                  (newBuffer (string-append data "\n"))
                  data)])
    (push this data)
    (void (increment-field! _offset this (buffer-length data)))))


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
    (hash-set! (· this _info data) key (if (string? val) (String val) val)))
  (· this _info end)

  ;; todo: fonts
  ;; for name, font of @_fontFamilies
  ;; font.finalize()

  (· this _root end)
  (· this _root data Pages end)

  (if (zero? (· this _waiting))
      (· this _finalize)
      (set-field! _ended this #t))

  #t)


(define/contract (_finalize this [fn #f])
  (() ((or/c procedure? #f)) . ->*m . void?)
  ;; generate xref
  (define xref-offset (· this _offset))
  (_write this "xref")
  (_write this (format "0 ~a" (add1 (length (· this _offsets)))))
  (_write this "0000000000 65535 f ")
  (for ([offset (in-list (· this _offsets))])
    (_write this (string-append
                  (~r offset #:min-width 10 #:pad-string "0")
                  " 00000 n ")))
  ;; trailer
  (_write this "trailer")
  
  (_write this (convert
                (mhash 'Size (add1 (length (· this _offsets)))
                       'Root (· this _root)
                       'Info (· this _info))))

  (_write this "startxref")
  (_write this (number xref-offset))
  (_write this "%%EOF")

  ;; end the stream
  ;; in node you (@push null) which signals to the stream
  ;; to copy to its output port
  ;; here we'll do it manually
  (copy-port (open-input-bytes (apply bytes-append (reverse (· this byte-strings)))) (· this op))
  (close-output-port (· this op)))