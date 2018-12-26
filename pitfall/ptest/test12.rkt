#lang racket/base
(require pitfall/pdftest)

(define-runtime-path charter-path "assets/charter.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "Charter" (path->string charter-path))

  ;; Set the font, draw some text
  [font doc "Charter"]
  [font-size doc 25]
  [text doc "Some text with an embedded font" 100 100 (hash
                                                       'width #f)])

;; test against non-subsetted font version
(define-runtime-path this "test12rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test12crkt.pdf")
(make-doc that #t proc)