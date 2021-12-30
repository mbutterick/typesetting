#lang racket/base
(require pitfall/pdftest)

;; subset OTF font
(define-runtime-path fira "assets/fira.otf")
(define-runtime-path charter "assets/charter.otf")

;; embed otf

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "fira" (path->string fira))
  (register-font doc "charter" (path->string charter))

  ;; Set the font, draw some text
  [font doc "fira"]
  [font-size doc 40]
  [text doc "Fira OTF rifle fire" 100 100])

;; test against non-subsetted font version
(define-runtime-path this "test20rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test20crkt.pdf")
(make-doc that #t proc)

(define-runtime-path the-other "test20.pdf")
(check-font-subsets-equal? this the-other)

