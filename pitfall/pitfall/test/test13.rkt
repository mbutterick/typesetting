#lang pitfall/pdftest

(define-runtime-path charter-path "assets/charter.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "Charter" (path->string charter-path))

  ;; Set the font, draw some text
  (send* doc
    [font "Charter"]
    [fontSize 25]
    [text "Åcçénts äre în" 100 100 (hash 'width #f)]))

;; test against non-subsetted font version
(define-runtime-path this "test13rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test13crkt.pdf")
(make-doc that #t proc #:pdfkit #f)

#;(module+ test
  (define doc (make-object PDFDocument))
  (send doc registerFont "Charter" (path->string charter-path))
  (send* doc [font "Charter"])
  (send doc pipe (open-output-string))
  (send doc end))
