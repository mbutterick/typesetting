#lang debug racket/base
(require pitfall/pdftest)

;; subset OTF font
(define-runtime-path sarabun "assets/sarabun.ttf")

;; embed otf

(define (proc doc)
  (register-font doc "sarabun" (path->string sarabun))

  ;; Set the font, draw some text
  [font doc "sarabun"]
  [font-size doc 40]
  [text doc "hello สวัสดีชาวโลก world" 100 150])

;; test against non-subsetted font version
(define-runtime-path this "test23rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test23crkt.pdf")
(make-doc that #t proc)

