#lang racket/base
(require pitfall/pdftest)

(define-runtime-path charter-path "assets/charter.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "Charter" (path->string charter-path))

  ;; Set the font, draw some text
  [font doc "Charter"]
  [font-size doc 25]
  [text doc "Åcçénts äre în" 100 100])

;; test against non-subsetted font version
(define-runtime-path this "test13rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test13crkt.pdf")
(make-doc that #t proc)
