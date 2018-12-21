#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/format
  racket/generator
  racket/match
  racket/list
  sugar/unstable/dict
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

(define PDFDocument
  (class (annotation-mixin (image-mixin (text-mixin (fonts-mixin (color-mixin (vector-mixin object%))))))
    (super-new)
    (init-field [(@options options) (mhasheq)])  
    (field [@pages null]
           [@offsets (mhasheqv)] ; The PDF object stores
           [ref-gen (generator ()
                               (let loop ([refid 1])
                                 (hash-set! @offsets refid #f)
                                 (yield refid)
                                 (loop (add1 refid))))]
           [(@root _root) (ref (mhasheq 'Type "Catalog"
                                        'Pages (ref (mhasheq 'Type "Pages"
                                                             'Count 0
                                                             'Kids empty))))] ; top object
           [(@x x) 0]
           [(@y y) 0]
           [(@info info) (mhasheq
                          'Producer "PITFALL"
                          'Creator "PITFALL"
                          'CreationDate (seconds->date (if (test-mode)
                                                           0
                                                           (current-seconds)) #f))])  ; Initialize the metadata

    ;; Initialize mixins
    (send this initColor)
    (send this initVector)
    (inherit-field _ctm)
    (send this initFonts)
    (inherit-field @font-families)
    (send this initText)
    (send this initImages)

    ;; initialize params
    (current-compress-streams? (hash-ref @options 'compress #t))
    (current-auto-first-page (hash-ref @options 'autoFirstPage #t))
    (current-doc-offset 0)

    (define/public (page) (first @pages))

    (define/public (ref [payload (mhasheq)])
      (make-object PDFReference this (ref-gen) payload))

    (define/public (write x)
      (define bstr (if (bytes? x) x (string->bytes/latin-1 (string-append x "\n"))))
      (write-bytes bstr)
      (current-doc-offset (file-position (current-output-port))))
      
    (define/public (addPage [options-arg @options])
      ;; end the current page if needed
      (unless (hash-ref @options 'bufferPages #f)
        (flush-pages))

      ;; create a page object
      (set! @pages (cons (make-object PDFPage this options-arg) @pages))
  
      ;; in Kids, store page dictionaries in correct order
      ;; this determines order in document
      (define pages (get-field payload (hash-ref (get-field payload @root) 'Pages)))
      (hash-update! pages 'Kids (λ (val) (append val (list (get-field dictionary (page))))))
      (hash-set! pages 'Count (length (hash-ref pages 'Kids)))

      ;; reset x and y coordinates
      (set! @x (hash-ref (get-field margins (page)) 'left))
      (set! @y (hash-ref (get-field margins (page)) 'top))
      ;; flip PDF coordinate system so that the origin is in
      ;; the top left rather than the bottom left
      (set! _ctm default-ctm-value)
      (send this transform 1 0 0 -1 0 (get-field height (page)))
      this)

    (define/public (flush-pages)
      (for-each (λ (p) (send p end)) @pages)
      (set! @pages empty))

    (define/public (addContent data)
      (send (page) write data)
      this)

    (define/public (_refEnd aref)
      (hash-set! @offsets (get-field id aref) (get-field offset aref)))

    (define/public (end) ; called from source file to finish doc
      (write (format "%PDF-~a" (current-pdf-version)))
      (write (string-append "%" (list->string (map integer->char (make-list 4 #xFF)))))
      (flush-pages)
      (define doc-info (ref))
      (for ([(key val) (in-hash @info)])
        ;; upgrade string literal to String struct
        (hash-set! (get-field payload doc-info) key (if (string? val) (String val) val)))
      (send doc-info end)
    
      (for ([font (in-hash-values @font-families)])
        (send font finalize))
      (send @root end)
      (send (hash-ref (get-field payload @root) 'Pages) end)

      (define xref-offset (current-doc-offset))
      (match-define (list this-idxs this-offsets)
        (match (sort (hash->list @offsets) < #:key car) ; sort by refid
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
        (write (string-append (~r offset #:min-width 10 #:pad-string "0") " 00000 n ")))
    
      (write "trailer")
      (write (convert
              (mhash 'Size (add1 (length this-offsets))
                     'Root @root
                     'Info doc-info)))
      (write "startxref")
      (write (numberizer xref-offset))
      (write "%%EOF"))

    ; if no 'info key, nothing will be copied from (hash)
    (for ([(key val) (in-hash (hash-ref @options 'info (hash)))]) 
      (hash-set! @info key val))

    ;; Add the first page
    (when (current-auto-first-page) (addPage))))

  (module+ test
    (define d (new PDFDocument)))