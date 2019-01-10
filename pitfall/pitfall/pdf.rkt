#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/match
  racket/format
  racket/dict
  sugar/unstable/dict
  "annotation.rkt"
  "reference.rkt"
  "object.rkt"
  "page.rkt"
  "vector.rkt"
  "font.rkt")
(provide (all-defined-out))

(define (store-ref doc ref)
  (set-pdf-refs! doc (cons ref (pdf-refs doc))))

(define (make-pdf [options (make-hasheq)]
                  #:output-path [output-path #f]
                  #:compress [compress? (current-compress-streams)]
                  #:auto-first-page [auto-first-page? (current-auto-first-page)])

  ;; initial values
  (define pages null)
  (define refs null)
  (define info (mhasheq 'Producer "PITFALL"
                        'Creator "PITFALL"
                        'CreationDate (current-seconds)))
  (for ([(key val) (in-hash (hash-ref options 'info (hasheq)))]) 
       (hash-set! info key val))
  (define opacity-registry (make-hash))
  (define current-fill-color '("black" 1))
  (define ctm default-ctm-value)
  (define ctm-stack null)
  (define font-families (make-hash))
  (define current-font-features null)
  (define current-font-size 12)
  (define current-font #false)
  (define registered-fonts (make-hash))
  (define line-gap 0)
  (define x 0)
  (define y 0)
  (define image-registry (make-hash))
  (define new-doc (pdf options
                       pages
                       refs
                       'dummy-root-value-that-will-be-replaced-below
                       info
                       opacity-registry
                       current-fill-color
                       ctm
                       ctm-stack
                       font-families
                       current-font-features
                       current-font-size
                       current-font
                       registered-fonts
                       line-gap
                       x
                       y
                       image-registry
                       output-path))
  (set-current-ref-id! 1)
  (reset-annotations-cache!)
  (register-ref-listener (λ (ref) (store-ref new-doc ref)))
  (set-pdf-root! new-doc (make-ref (mhasheq 'Type 'Catalog
                                            'Pages (make-ref (mhasheq 'Type 'Pages)))))

  ;; initialize params
  (current-compress-streams compress?)
  (current-auto-first-page auto-first-page?)
  (when (current-auto-first-page) (add-page new-doc))
  (when (current-auto-helvetica) (font new-doc "Helvetica"))
  new-doc)

(define (add-page doc [options-arg (pdf-options doc)])
  ;; create a page object
  (define page-parent (dict-ref (pdf-root doc) 'Pages))
  (set-pdf-pages! doc (cons (make-page page-parent options-arg) (pdf-pages doc)))
      
  ;; reset x and y coordinates
  (set-pdf-x! doc (margin-left ($page-margins (current-page doc))))
  (set-pdf-y! doc (margin-right ($page-margins (current-page doc))))
  ;; flip PDF coordinate system so that the origin is in
  ;; the top left rather than the bottom left
  (set-pdf-ctm! doc default-ctm-value)
  (transform doc 1 0 0 -1 0 ($page-height (current-page doc)))
  doc)

(define last-output-port #f)

(define (start-doc doc)
  (define output-port (match (pdf-output-path doc)
                        [(? path-string? ps) (open-output-file ps #:exists 'replace)]
                        [(? output-port? op) op]
                        [#false (current-output-port)]))
  (set! last-output-port (current-output-port))
  (current-output-port output-port)
  (write-bytes-out (format "%PDF-~a" (current-pdf-version)))
  (write-bytes-out "%ÿÿÿÿ"))

(define (end-doc doc)
  (for-each page-end (pdf-pages doc))

  (define doc-info (make-ref (pdf-info doc)))
  (ref-end doc-info)
    
  (for-each font-end (hash-values (pdf-font-families doc)))

  (define pages-ref (dict-ref (pdf-root doc) 'Pages))
  (dict-set! pages-ref 'Count (length (pdf-pages doc)))
  (dict-set! pages-ref 'Kids (map $page-ref (reverse (pdf-pages doc))))
  (ref-end pages-ref)
      
  (ref-end (pdf-root doc))
      
  (define xref-offset (file-position (current-output-port)))
  (write-bytes-out "xref")
  (define xref-count (add1 (length (pdf-refs doc))))
  (write-bytes-out (format "0 ~a" xref-count))
  (write-bytes-out "0000000000 65535 f ")
  (for ([ref (in-list (reverse (pdf-refs doc)))])
       (write-bytes-out
        (string-append (~r ($ref-offset ref) #:min-width 10 #:pad-string "0") " 00000 n ")))
  (write-bytes-out "trailer")
  (write-bytes-out (convert (mhasheq 'Size xref-count
                                     'Root (pdf-root doc)
                                     'Info doc-info)))
  (write-bytes-out "startxref")
  (write-bytes-out (numberizer xref-offset))
  (write-bytes-out "%%EOF")
  (close-output-port (current-output-port))
  (current-output-port last-output-port))

(module+ test
  (define d (make-pdf)))