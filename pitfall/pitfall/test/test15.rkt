#lang pitfall/pdftest

(define-runtime-path ttf-path "assets/eqbi.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 25]
    [text "Hello World" 100 100 (hash 'width #f)]))

;; test against non-subsetted font version
(define-runtime-path this "test14rkt.pdf")
(make-doc this #f proc #:pdfkit #f)

(define-runtime-path that "test14crkt.pdf")
(make-doc that #t proc #:pdfkit #f)

#;(module+ test
  (define doc (make-object PDFDocument))
  (send doc registerFont "Charter" (path->string charter-path))
  (send* doc [font "Charter"])
  (send doc pipe (open-output-string))
  (send doc end))
