#lang br
(require racket/draw)
(provide PDFDocument)

(require "reference.rkt")
;(require "page.rkt")

(define PDFDocument
  (class object% ; actually is an instance of readable.Stream, which is an input port
    (init-field [options (hasheq)])
    (let ([output-file (hash-ref options 'out "outrkt.pdf")])
      (super-new))

    ; list of byte chunks to push onto
    ; simulates interface of stream.readable
    (field [byte-strings empty]) 
    
    ;; PDF version
    (field [version 1.3])
    
    ;; Whether streams should be compressed
    (field [compress (hash-ref options 'compress #t)])
    
    (field [_pageBuffer null])
    (field [_pageBufferStart 0])

    ;; The PDF object store
    (field [_offsets null])
    (field [_waiting 0])
    (field [_ended #f])
    (field [_offset 0])

    (field [_root (ref
                   (hasheq 'Type 'Catalog'
                           'Pages (ref
                                   (hasheq 'Type 'Pages'
                                           'Count 0
                                           'Kids empty))))])

    ;; The current page
    (field [page #f])

    ;; todo
    ;; Initialize mixins
    #;(initColor)
    #;(initVector)
    #;(initFonts)
    #;(initText)
    #;(initImages)

    ;; Initialize the metadata
    (field [info (hasheq
                  'Producer 'PitfallKit'
                  'Creator 'PitfallKit'
                  'CreationDate (seconds->date (current-seconds)))])

    (when (hash-ref options 'info #f)
      (for ([(key val) (in-hash (hash-ref options 'info))])
        (hash-set! info key val)))

    ;; Write the header
    ;; PDF version
    (_write (format "%PDF-~a" version))

    ;; 4 binary chars, as recommended by the spec
    (let ([c (integer->char #xFF)])
      (_write (string-append "%" (string c c c c))))

    ;; Add the first page
    #;(unless (not (hash-ref options 'autoFirstPage #t))
        (addPage)) ; todo

    ;; todo
    ;;mixin = (methods) =>
    ;;for name, method of methods
    ;;this::[name] = method

    ;; todo
    ;; Load mixins
    ;mixin require './mixins/color'
    ;mixin require './mixins/vector'
    ;mixin require './mixins/fonts'
    ;mixin require './mixins/text'
    ;mixin require './mixins/images'
    ;mixin require './mixins/annotations'

    (field [x 0])
    (field [y 0])
    (field [_ctm null])
    (define/public (addPage [options options])
      ;; end the current page if needed
      (unless (hash-ref options 'bufferPages #f)
        (flushPages))

      ;; create a page object
      (define page 42) ; todo new PDFPage(this, options)
      (push! _pageBuffer page)

      ;; add the page to the object store
      (define pages (make-hasheq)) ; todo @_root.data.Pages.data
      (hash-update! pages 'Kids (λ (val) (cons 42 val)) null) ; todo @page.dictionary
      (hash-update! pages 'Count add1 0)

      ;; reset x and y coordinates
      (set! x 42) ;; todo @page.margins.left
      (set! y 42) ;; todo @page.margins.top

      ;; flip PDF coordinate system so that the origin is in
      ;; the top left rather than the bottom left
      (set! _ctm '(1 0 0 1 0 0))
      ;; (transform 1 0 0 -1 0 @page.height) ;; todo

      ;; @emit('pageAdded') ;; todo

      this)
    
    (define/public (flushPages)
      42) ; temp
    
    (define/public (ref data)
      (define ref (make-object PDFReference this (add1 (length _offsets)) data))
      (push! _offsets #f) ; placeholder for this object's offset once it is finalized
      (set! _waiting (add1 _waiting))
      ref
      42)

    (define/public (push chunk)
      (set! byte-strings (cons chunk byte-strings)))
                                          
    (define/public (_write data)
      (let ([data (if (not (bytes? data))
                      ; `string->bytes/latin-1` is equivalent to plain binary encoding
                      (string->bytes/latin-1 (string-append data "\n"))
                      data)])
        (push data)
        (report byte-strings)
        (set! _offset (+ _offset (bytes-length data)))))

    (define/public (pipe op)
      (copy-port (open-input-bytes (apply bytes-append (reverse byte-strings))) op)
      (close-output-port op))

    (define _info #f)
    (define/public (end)
      (flushPages)
      'done))) ; temp


(define doc (new PDFDocument))

(module+ test
  (require rackunit)
  (send doc _write "foobar")
  (send doc pipe (open-output-file "testrkt0.pdf" #:exists 'replace))
  (check-equal? (send doc end) 'done))