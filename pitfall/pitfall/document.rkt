#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/format
  racket/dict
  sugar/unstable/dict
  "reference.rkt"
  "object.rkt"
  "page.rkt"
  "vector.rkt"
  "fonts.rkt")
(provide (all-defined-out))

(define (store-ref doc ref)
  (set-$doc-refs! doc (cons ref ($doc-refs doc))))

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
  (define current-fill-color #false)
  (define ctm default-ctm-value)
  (define ctm-stack null)
  (define font-families (make-hash))
  (define current-font-size 12)
  (define current-font #false)
  (define registered-fonts (make-hash))
  (define line-gap 0)
  (define text-options #false)
  (define x 0)
  (define y 0)
  (define image-registry (make-hash))
  (define new-doc ($doc options
                        pages
                        refs
                        'dummy-root-value-that-will-be-replaced-below
                        info
                        opacity-registry
                        current-fill-color
                        ctm
                        ctm-stack
                        font-families
                        current-font-size
                        current-font
                        registered-fonts
                        line-gap
                        text-options
                        x
                        y
                        image-registry))
  (set-current-ref-id! 1)
  (register-ref-listener (λ (ref) (store-ref new-doc ref)))
  (set-$doc-root! new-doc (make-ref (mhasheq 'Type 'Catalog
                                             'Pages (make-ref (mhasheq 'Type 'Pages)))))

  ;; initialize params
  (current-compress-streams? (hash-ref options 'compress #t))
  (current-auto-first-page (hash-ref options 'autoFirstPage #t))
  (when (current-auto-first-page) (add-page new-doc))
  (when (current-auto-helvetica) (font new-doc "Helvetica"))
  
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

  (define doc-info (make-ref ($doc-info doc)))
  (ref-end doc-info)
    
  (for ([font (in-hash-values ($doc-font-families doc))])
    (send font font-end))

  (define pages-ref (dict-ref ($doc-root doc) 'Pages))
  (dict-set! pages-ref 'Count (length ($doc-pages doc)))
  (dict-set! pages-ref 'Kids (map $page-dictionary (reverse ($doc-pages doc))))
  (ref-end pages-ref)
      
  (ref-end ($doc-root doc))
      
  (define xref-offset (file-position (current-output-port)))
  (write-bytes-out "xref")
  (define xref-count (add1 (length ($doc-refs doc))))
  (write-bytes-out (format "0 ~a" xref-count))
  (write-bytes-out "0000000000 65535 f ")
  (for ([ref (in-list (reverse ($doc-refs doc)))])
    (write-bytes-out
     (string-append (~r ($ref-offset ref) #:min-width 10 #:pad-string "0") " 00000 n ")))
  (write-bytes-out "trailer")
  (write-bytes-out (convert (mhasheq 'Size xref-count
                                     'Root ($doc-root doc)
                                     'Info doc-info)))
  (write-bytes-out "startxref")
  (write-bytes-out (numberizer xref-offset))
  (write-bytes-out "%%EOF"))

(module+ test
  (define d (make-$doc)))