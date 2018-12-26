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
(provide (all-defined-out))

(define (store-ref doc ref)
  (set-$doc-refs! doc (cons ref ($doc-refs doc))))

(define (page doc) (first ($doc-pages doc)))

(define (add-content doc data)
  (page-write (page doc) data))

(define (transform doc scaleX shearY shearX scaleY mdx mdy)
  (define new-ctm (list scaleX shearY shearX scaleY mdx mdy))
  (set-$doc-ctm! doc (combine-transforms ($doc-ctm doc) new-ctm))
  (add-content doc (make-transform-string new-ctm)))

(define (make-$doc [options (make-hasheq)])

  ;; initial values
  (define pages null)
  (define refs null)
  (define info (mhasheq 'Producer "PITFALL"
                        'Creator "PITFALL"
                        'CreationDate (current-seconds)))
  (for ([(key val) (in-hash (hash-ref options 'info (hasheq)))]) 
    (hash-set! info key val))
  (define opacity-registry (make-hash))
  (define opacity-count 0)
  (define grad-count 0)
  (define current-fill-color #false)
  (define ctm default-ctm-value)
  (define ctm-stack null)
  (define font-families (make-hash))
  (define font-count 0)
  (define current-font-size 12)
  (define current-font #false)
  (define registered-fonts (make-hash))
  (define line-gap 0)
  (define text-options #false)
  (define x 0)
  (define y 0)
  (define image-registry (make-hash))
  (define image-count 0)
  (define new-doc ($doc options
                        pages
                        refs
                        'dummy-root-value-that-will-be-replaced-below
                        info
                        opacity-registry
                        opacity-count
                        grad-count
                        current-fill-color
                        ctm
                        ctm-stack
                        font-families
                        font-count
                        current-font-size
                        current-font
                        registered-fonts
                        line-gap
                        text-options
                        x
                        y
                        image-registry
                        image-count))
  (set-current-ref-id! 1)
  (register-ref-listener (λ (ref) (store-ref new-doc ref)))
  (set-$doc-root! new-doc (make-ref (mhasheq 'Type 'Catalog
                                             'Pages (make-ref (mhasheq 'Type 'Pages)))))

  ;; initialize params
  (current-compress-streams? (hash-ref options 'compress #t))
  (current-auto-first-page (hash-ref options 'autoFirstPage #t))
  (when (current-auto-first-page) (add-page new-doc))
  #;(when (current-auto-helvetica) (font "Helvetica"))
  
  new-doc)

(define (add-page doc [options-arg ($doc-options doc)])
  ;; create a page object
  (define page-parent (dict-ref ($doc-root doc) 'Pages))
  (set-$doc-pages! doc (cons (make-page page-parent options-arg) ($doc-pages doc)))
      
  ;; reset x and y coordinates
  (set-$doc-x! doc (margin-left ($page-margins (page doc))))
  (set-$doc-y! doc (margin-right ($page-margins (page doc))))
  ;; flip PDF coordinate system so that the origin is in
  ;; the top left rather than the bottom left
  (set-$doc-ctm! doc default-ctm-value)
  (transform doc 1 0 0 -1 0 ($page-height (page doc)))
  doc)

(define (start-doc doc)
  (write-bytes-out (format "%PDF-~a" (current-pdf-version)))
  (write-bytes-out "%ÿÿÿÿ"))

(define (end-doc doc)
  (for-each page-end ($doc-pages doc))

  (define doc-info (make-ref))
  (for ([(key val) (in-hash ($doc-info doc))])
    (dict-set! doc-info key val))
  (ref-end doc-info)
    
  (for ([font (in-hash-values ($doc-font-families doc))])
    (send font end))

  (define pages-ref (dict-ref ($doc-root doc) 'Pages))
  (dict-set! pages-ref 'Count (length ($doc-pages doc)))
  (dict-set! pages-ref 'Kids (map $page-dictionary (reverse ($doc-pages doc))))
  (ref-end pages-ref)
      
  (ref-end ($doc-root doc))
      
  (define xref-offset (file-position (current-output-port)))
  (write-bytes-out "xref")
  (write-bytes-out (format "0 ~a" (add1 (length ($doc-refs doc)))))
  (write-bytes-out "0000000000 65535 f ")
  (for ([ref (in-list (reverse ($doc-refs doc)))])
    (write-bytes-out
     (string-append (~r ($ref-offset ref) #:min-width 10 #:pad-string "0") " 00000 n ")))
  (write-bytes-out "trailer")
  (write-bytes-out (convert (mhasheq 'Size (add1 (length ($doc-refs doc)))
                                     'Root ($doc-root doc)
                                     'Info doc-info)))
  (write-bytes-out "startxref")
  (write-bytes-out (numberizer xref-offset))
  (write-bytes-out "%%EOF"))

#;(define PDFDocument
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
                                 (page-write (first @pages) data)))))))))
      (set-current-ref-id! 1)
      (register-ref-listener (λ (ref) (store-ref ref)))

      (super-new)
      (init-field [(@options options) (mhasheq)])
      (field [@refs null]
             [@root (make-ref (mhasheq 'Type 'Catalog
                                       'Pages (make-ref (mhasheq 'Type 'Pages))))]
             ;; initialize the metadata
             [@info (mhasheq 'Producer "PITFALL"
                             'Creator "PITFALL"
                             'CreationDate (current-seconds))])

      ;; initialize mixins
      (inherit-field @ctm) ; from vector mixin
      (inherit-field @font-families) (inherit font) ; from font mixin
      (inherit-field [@x x] [@y y]) ; from text
      (inherit transform) ; from vector
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
        (set! @pages (cons (make-page page-parent options-arg) @pages))
      
        ;; reset x and y coordinates
        (set! @x (margin-left ($page-margins (page))))
        (set! @y (margin-right ($page-margins (page))))
        ;; flip PDF coordinate system so that the origin is in
        ;; the top left rather than the bottom left
        (set! @ctm default-ctm-value)
        (transform 1 0 0 -1 0 ($page-height (page)))
        this)

      (define/public (start-doc)
        (write-bytes-out (format "%PDF-~a" (current-pdf-version)))
        (write-bytes-out "%ÿÿÿÿ"))

      (define/public (end-doc)
        (for-each page-end @pages)

        (define doc-info (make-ref))
        (for ([(key val) (in-hash @info)])
          (dict-set! doc-info key val))
        (ref-end doc-info)
    
        (for ([font (in-hash-values @font-families)])
          (send font end))

        (define pages-ref (dict-ref @root 'Pages))
        (dict-set! pages-ref 'Count (length @pages))
        (dict-set! pages-ref 'Kids (map $page-dictionary (reverse @pages)))
        (ref-end pages-ref)
      
        (ref-end @root)
      
        (define xref-offset (file-position (current-output-port)))
        (write-bytes-out "xref")
        (write-bytes-out (format "0 ~a" (add1 (length @refs))))
        (write-bytes-out "0000000000 65535 f ")
        (for ([ref (in-list (reverse @refs))])
          (write-bytes-out
           (string-append (~r ($ref-offset ref) #:min-width 10 #:pad-string "0") " 00000 n ")))
        (write-bytes-out "trailer")
        (write-bytes-out (convert (mhasheq 'Size (add1 (length @refs))
                                           'Root @root
                                           'Info doc-info)))
        (write-bytes-out "startxref")
        (write-bytes-out (numberizer xref-offset))
        (write-bytes-out "%%EOF"))))

(module+ test
  (define d (make-$doc)))