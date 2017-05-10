#lang br
(require racket/draw)
(provide PDFDocument)

(define PDFDocument
  (class pdf-dc%
    (init [opts (hasheq)])

    (define options opts)
    (define output-file (hash-ref options 'out "outrkt.pdf"))
    
    (super-new [interactive #f]	 
               [parent #f]	 
               [use-paper-bbox #f]	 
               [as-eps #f]	 
               [width #f]	 
               [height #f]	 
               [output (open-output-file output-file #:exists 'replace)])
    
    ;; PDF version
    (define version 1.3)
    
    ;; Whether streams should be compressed
    (define compress (hash-ref options 'compress #t))
    
    (define _pageBuffer null)
    (define _pageBufferStart 0)

    ;; The PDF object store
    (define _offsets null)
    (define _waiting 0)
    (define _ended #f)
    (define _offset 0)

    (define _root (ref
                   (hasheq 'Type 'Catalog'
                           'Pages (ref
                                   (hasheq 'Type 'Pages'
                                           'Count 0
                                           'Kids empty)))))

    ;; The current page
    (define page #f)

    ;; todo
    ;; Initialize mixins
    #;(initColor)
    #;(initVector)
    #;(initFonts)
    #;(initText)
    #;(initImages)

    ;; Initialize the metadata
    (define info (hasheq
                  'Producer 'PitfallKit'
                  'Creator 'PitfallKit'
                  'CreationDate (seconds->date (current-seconds))))

    (when (hash-ref options 'info #f)
      (for ([(key val) (in-hash (hash-ref options 'info))])
           (hash-set! info key val)))

    ;; Write the header
    ;; PDF version
    (_write (format "%PDF-#{~a}" version))

    (_write (format "%~a~a~a~a" #xFF #xFF #xFF #xFF))

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

    (define x #f)
    (define y #f)
    (define _ctm null)
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
    
    (define/public (ref . xs)
      42) ; temp

    (define/public (_write . xs)
      42) ; temp

    (define (end-doc) 'done) ; tempo
    (override end-doc)

    (define _info #f)
    (define/public (end)
      (flushPages)
      (end-doc)))) ; temp


(define doc (make-object PDFDocument (hasheq 'out "testrkt0.pdf")))
(module+ test
  (require rackunit)
  (check-equal? (send doc end) 'done))