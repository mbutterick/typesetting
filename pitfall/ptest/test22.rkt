#lang debug racket/base
(require pitfall/pdftest)

;; subset OTF font
(define-runtime-path fira "assets/fira.otf")
(define-runtime-path noto "assets/noto-emoji/NotoEmoji-Regular.ttf")

;; embed otf

(define (proc doc)
  ;; Use a font with 2048 em (noto)
  (register-font doc "noto" (path->string noto))

  ;; Set the font, draw some text
  [font doc "noto"]
  [font-size doc 40]
  [text doc "😂😂😂" 100 150])

;; test against non-subsetted font version
(define-runtime-path this "test22rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test22crkt.pdf")
(make-doc that #t proc)

