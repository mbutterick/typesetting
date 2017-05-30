#lang pitfall/pdftest

(define-runtime-path charter-path "assets/charter.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "Charter" (path->string charter-path))

  ;; Set the font, draw some text
  (send* doc
    [font "Charter"]
    [fontSize 25]
    [text "Some text with an embedded font" 100 100 (hash
                                                       'width #f)]))

(define-runtime-path this "test12rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test12crkt.pdf")
#;(make-doc that #t proc #:test #f)

(module+ test
  (define doc (make-object PDFDocument))
  (send doc registerFont "Charter" (path->string charter-path))
  (send* doc [font "Charter"])
  #;doc)
