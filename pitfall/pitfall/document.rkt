#lang debug at-exp racket/base
(require
  "param.rkt"
  "struct.rkt"
  racket/class
  racket/format
  racket/contract
  racket/generator
  racket/match
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

(define-subclass mixed% (PDFDocument [options (mhash)])
  (current-compress-streams? (hash-ref options 'compress #t))
  (current-auto-first-page (hash-ref options 'autoFirstPage #t))
  (current-doc-offset 0)
  
  (field [_pageBuffer null]
         [_offsets (mhasheqv)] ; The PDF object store
         [_ended #f]
         [ref-gen (generator ()
                             (let loop ([refid 1])
                               (hash-set! _offsets refid 'missing-ref-offset)
                               (yield refid)
                               (loop (add1 refid))))]
         [_root (ref (mhasheq 'Type "Catalog"
                              'Pages (ref (mhasheq 'Type "Pages"
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
         ) ; for `pipe`
        

  ;; Initialize mixins
  (· this initColor)
  (· this initVector)
  (· this initFonts)
  (inherit-field _fontFamilies)
  (· this initText)
  (· this initImages)

  (define/public (ref [payload (mhash)])
    (make-object PDFReference this (ref-gen) payload))

  (define/public (write x)
    (define bstr (if (not (bytes? x))
                     (string->bytes/latin-1 (string-append x "\n"))
                     x))
    (write-bytes bstr)
    (current-doc-offset (+ (current-doc-offset) (bytes-length bstr))))
      
  (define/public (addPage [options-arg options])
    ;; end the current page if needed
    (unless (hash-ref options 'bufferPages #f)
      (flushPages))

    ;; create a page object
    (set! page (make-object PDFPage this options-arg))
    (set! _pageBuffer (cons page _pageBuffer))
  
    ;; in Kids, store page dictionaries in correct order
    ;; this determines order in document
    (define pages (· _root payload Pages payload))
    (hash-update! pages 'Kids (λ (val) (append val (list (· page dictionary)))))
    (hash-set! pages 'Count (length (hash-ref pages 'Kids)))

    ;; reset x and y coordinates
    (set! x (· page margins left))
    (set! y (· page margins top))
    ;; flip PDF coordinate system so that the origin is in
    ;; the top left rather than the bottom left
    (set-field! _ctm this default-ctm-value)
    (send this transform 1 0 0 -1 0 (· this page height))
    this)

  (define/public (flushPages)
    (for-each (λ (p) (· p end)) _pageBuffer)
    (set! _pageBuffer empty))

  (define/public (addContent data)
    (send page write data)
    this)

  (define/public (_refEnd aref)
    (hash-set! _offsets (· aref id) (· aref offset)))

  (define/public (end) ; called from source file to finish doc
    (flushPages)
    (define _info (ref))
    (for ([(key val) (in-hash info)])
      ;; upgrade string literal to String struct
      (hash-set! (· _info payload) key (if (string? val) (String val) val)))

    (· _info end)

    (for ([font (in-hash-values _fontFamilies)])
      (· font finalize))

    (· _root end)
    (· _root payload Pages end)

    (define xref-offset (current-doc-offset))
    (match-define (list this-idxs this-offsets)
      (match (sort (hash->list _offsets) < #:key car) ; sort by refid
        [(list (cons idxs offsets) ...) (list idxs offsets)]))
    (write "xref")
    (write (format "0 ~a" (add1 (length this-offsets))))
    (write "0000000000 65535 f ")
    (let ([missing-offsets (for/list ([offset (in-list this-offsets)]
                                      [idx (in-list this-idxs)]
                                      #:unless (number? offset))
                             idx)])
      (unless (empty? missing-offsets)
        (raise-argument-error 'document:end "numerical offsets" missing-offsets)))
    (for ([offset (in-list this-offsets)]
          [idx (in-list this-idxs)])
      (write @string-append{@(~r offset #:min-width 10 #:pad-string "0") 00000 n }))
    
    (write "trailer")
    (write (convert
            (mhash 'Size (add1 (length this-offsets))
                   'Root _root
                   'Info _info)))
    (write "startxref")
    (write (number xref-offset))
    (write "%%EOF"))

  ; if no 'info key, nothing will be copied from (hash)
  (for ([(key val) (in-hash (hash-ref options 'info (hash)))]) 
    (hash-set! info key val))

  ;; Write the header
  (write (format "%PDF-~a" (current-pdf-version))) ;  PDF version
  (write (string-append "%" (list->string (map integer->char (make-list 4 #xFF))))) ; 4 binary chars, as recommended by the spec

  ;; Add the first page
  (when (current-auto-first-page) (addPage)))

(module+ test
  (define d (new PDFDocument)))