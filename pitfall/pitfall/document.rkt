#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/format
  racket/dict
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
  (class (annotation-mixin
          (image-mixin
           (text-mixin
            (fonts-mixin
             (vector-mixin
              (color-mixin (class object%
                             (super-new)
                             (field [@pages null])
                             (define/public (page) (first @pages))                  
                             (define/public (add-content data)
                               (send (first @pages) write data)
                               this))))))))
    (set-current-ref-id! 1)
    (register-ref-listener (λ (ref) (send this store-ref ref)))

    (super-new)
    (init-field [(@options options) (mhasheq)])
    (field [@refs null]
           [@root (make-ref (mhasheq 'Type 'Catalog
                                     'Pages (make-ref (mhasheq 'Type 'Pages))))]
           ;; initialize the metadata
           [@info (mhasheq 'Producer "PITFALL"
                           'Creator "PITFALL"
                           'CreationDate (seconds->date (if (test-mode) 0 (current-seconds)) #f))])

    ;; initialize mixins
    (inherit-field @ctm) ; from vector mixin
    (inherit-field @font-families) (inherit font) ; from font mixin
    (inherit-field [@x x] [@y y]) ; from text
    (inherit-field @pages) (inherit page) ; from base

    ;; initialize params
    (current-compress-streams? (hash-ref @options 'compress #t))
    (current-auto-first-page (hash-ref @options 'autoFirstPage #t))
    (when (current-auto-first-page) (add-page))
    (when (current-auto-helvetica) (font "Helvetica"))

    ;; copy options
    (for ([(key val) (in-hash (hash-ref @options 'info (hasheq)))]) 
      (hash-set! @info key val))

    (define/public (store-ref ref)
      (set! @refs (cons ref @refs)))

    (define/public (add-page [options-arg @options])
      ;; create a page object
      (define page-parent (dict-ref @root 'Pages))
      (set! @pages (cons (make-object PDFPage page-parent options-arg) @pages))
      
      ;; reset x and y coordinates
      (set! @x (margin-left (get-field margins (page))))
      (set! @y (margin-right (get-field margins (page))))
      ;; flip PDF coordinate system so that the origin is in
      ;; the top left rather than the bottom left
      (set! @ctm default-ctm-value)
      (send this transform 1 0 0 -1 0 (get-field height (page)))
      this)

    (define/public (end)
      (write-bytes-out (format "%PDF-~a" (current-pdf-version)))
      (write-bytes-out "%ÿÿÿÿ")

      (for ([page (in-list @pages)])
        (send page end))

      (define doc-info (make-ref))
      (for ([(key val) (in-hash @info)])
        (dict-set! doc-info key val))
      (send doc-info end)
    
      (for ([font (in-hash-values @font-families)])
        (send font end))

      (send* (dict-ref @root 'Pages)
        [set-key! 'Count (length @pages)]
        [set-key! 'Kids (map (λ (page) (get-field dictionary page)) (reverse @pages))]
        [end])
      
      (send @root end)
      
      (define xref-offset (file-position (current-output-port)))
      (write-bytes-out "xref")
      (write-bytes-out (format "0 ~a" (add1 (length @refs))))
      (write-bytes-out "0000000000 65535 f ")
      (for ([ref (in-list (reverse @refs))])
        (write-bytes-out
         (string-append (~r (get-field offset ref) #:min-width 10 #:pad-string "0") " 00000 n ")))
      (write-bytes-out "trailer")
      (write-bytes-out (convert (mhasheq 'Size (add1 (length @refs))
                                         'Root @root
                                         'Info doc-info)))
      (write-bytes-out "startxref")
      (write-bytes-out (numberizer xref-offset))
      (write-bytes-out "%%EOF"))))

(module+ test
  (define d (new PDFDocument)))