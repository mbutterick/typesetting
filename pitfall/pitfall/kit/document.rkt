#lang racket/base
(require racket/class racket/draw br/list racket/list racket/format racket/port)
(require sugar/debug)
(provide PDFDocument)

(require "reference.rkt" "struct.rkt" "object.rkt" "page.rkt" "helper.rkt")
(require "mixins/vector.rkt")

(define PDFDocument
  ;; actually is an instance of readable.Stream, which is an input port
  (class (vector-mixin object%)
    (init-field [(@options options) (mhash)])
    (let ([output-file (hash-ref @options 'out "outrkt.pdf")])
      (super-new))

    ; list of byte chunks to push onto
    ; simulates interface of stream.readable
    (field [(@byte-strings byte-strings) empty]) 
    
    ;; PDF version
    (field [(@version version) 1.3])
    
    ;; Whether streams should be compressed
    (field [(@compress compress) (hash-ref @options 'compress #t)])
    
    (field [(@_pageBuffer _pageBuffer) null])
    (field [(@_pageBufferStart _pageBufferStart) 0])

    ;; The PDF object store
    (field [(@_offsets _offsets) null])
    (field [(@_waiting _waiting) 0])
    (field [(@_ended _ended) #f])
    (field [(@_offset _offset) 0])

    (field [(@_root _root) (@ref
                            (mhash 'Type "Catalog"
                                   'Pages (@ref
                                           (mhash 'Type "Pages"
                                                  'Count 0
                                                  'Kids empty))))])

    ;; The current page
    (field [(@page page) #f])

    ;; other fields, hoisted from below (why is this necessary?)
    (field [(@x x) 0])
    (field [(@y y) 0])

    ;; todo
    ;; Initialize mixins
    #;(@initColor)
    (· this initVector)
    #;(@initFonts)
    #;(@initText)
    #;(@initImages)

    ;; Initialize the metadata
    (field [(@info info) (mhash
                          'Producer "PitfallKit"
                          'Creator "PitfallKit"
                          'CreationDate (seconds->date (current-seconds)))])

    (when (hash-ref @options 'info #f)
      (for ([(key val) (in-hash (hash-ref @options 'info))])
           (hash-set! @info key val)))

    ;; Write the header
    ;; PDF version
    (@_write (format "%PDF-~a" @version))

    ;; 4 binary chars, as recommended by the spec
    (let ([c (integer->char #xFF)])
      (@_write (string-append "%" (string c c c c))))

    ;; Add the first page
    (unless (not (hash-ref @options 'autoFirstPage #t))
      (@addPage))

    ;; todo
    ;;mixin = (methods) =>
    ;;for name, method of methods
    ;;this::[name] = method

    ;; todo
    ;; Load mixins
    ;; (in racket this is handled automatically in the class decl) 
    ;mixin require './mixins/color'
    ;mixin require './mixins/vector'
    ;mixin require './mixins/fonts'
    ;mixin require './mixins/text'
    ;mixin require './mixins/images'
    ;mixin require './mixins/annotations'

    
    (public [@addPage addPage])
    (define (@addPage [options @options])
      ;; end the current page if needed
      (unless (hash-ref @options 'bufferPages #f)
        (@flushPages))

      ;; create a page object
      (set! @page (make-object PDFPage this options))
      (push! @_pageBuffer @page)
      ;; add the page to the object store
      (define pages (· @_root data Pages data))
      (hash-update! pages 'Kids (λ (val) (cons (· @page dictionary) val)) null)
      (hash-update! pages 'Count add1)

      ;; reset x and y coordinates
      (set! @x (· @page margins left))
      (set! @y (· @page margins top))
      ;; flip PDF coordinate system so that the origin is in
      ;; the top left rather than the bottom left
      (set-field! _ctm this '(1 0 0 1 0 0))
      (send this transform 1 0 0 -1 0 (· @page height))

      #;(@emit "pageAdded") ; from eventemitter interface
      this
      )

    (public [@flushPages flushPages])
    (define (@flushPages)
      ;; this local variable exists so we're future-proof against
      ;; reentrant calls to flushPages.
      (define pages @_pageBuffer)
      (set! @_pageBuffer empty)
      (+= @_pageBufferStart (length pages))
      (·map end pages))

    ;; every js function argument is 'undefined' by default
    ;; so even a function defined without default values
    ;; can be called without arguments
    (public [@ref ref])
    (define (@ref [data (make-hasheq)])
      (define ref (make-object PDFReference this (add1 (length @_offsets)) data))
      (push! @_offsets #f) ; placeholder for this object's offset once it is finalized
      (++ @_waiting)
      ref)

    (public [@push push])
    (define (@push chunk)
      (push! @byte-strings chunk))

    (public [@_write _write])
    (define (@_write data)
      (let ([data (if (not (isBuffer? data))
                      (newBuffer (string-append data "\n"))
                      data)])
        (@push data)
        (+= @_offset (buffer-length data))))

    (public [@addContent addContent])
    (define (@addContent data)
      (send @page write data)
      this)

    (field [op #f])
    (define/public (pipe port)
      (set! op port))

    (field [(@_info _info) #f])
    (define/public (end)
      (@flushPages)
      (set! @_info (@ref))
      (for ([(key val) (in-hash @info)])
           ;; upgrade string literal to String struct
           (hash-set! (· @_info data) key (if (string? val) (String val) val)))
      (· @_info end)

      ;; todo: fonts
      ;; for name, font of @_fontFamilies
      ;; font.finalize()

      (· @_root end)
      (· @_root data Pages end)

      (if (or (zero? @_waiting) 'debug)
          (@_finalize)
          (set! @_ended #t))

      'done)

    (public (@_finalize _finalize))
    (define (@_finalize [fn #f])
      ;; generate xref
      (define xRefOffset @_offset)
      (@_write "xref")
      (@_write (format "0 ~a" (add1 (length @_offsets))))
      (@_write "0000000000 65535 f ")
      (for ([offset (in-list @_offsets)])
           (@_write (string-append
                     (~r (or offset #xdead) #;debug #:min-width 10 #:pad-string "0")
                     " 00000 n ")))
      ;; trailer
      (@_write "trailer")
      ;; todo: make `PDFObject:convert` a static method
      (@_write (send (make-object PDFObject) convert
                     (mhash 'Size (add1 (length @_offsets))
                            'Root @_root
                            'Info @_info)))

      (@_write "startxref")
      (@_write (number->string xRefOffset))
      (@_write "%%EOF")

      ;; end the stream
      ;; in node you (@push null) which signals to the stream
      ;; to copy to its output port
      ;; here we'll do it manually
      (copy-port (open-input-bytes (apply bytes-append (reverse @byte-strings))) op)
      (close-output-port op))))


(define doc (new PDFDocument))

(module+ test
  (require rackunit racket/file)
  (define ob (open-output-bytes))
  (send doc pipe ob)
  (check-equal? (send doc end) 'done)
  (check-equal? (get-output-bytes ob) (file->bytes "demo.pdf")))