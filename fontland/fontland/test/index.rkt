#lang racket
(require rackunit racket/runtime-path fontland)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/test/index.js
|#

(define-runtime-path open-sans-ttf "data/OpenSans/OpenSans-Regular.ttf")
(define-runtime-path source-sans-otf "data/SourceSansPro/SourceSansPro-Regular.otf")
(define-runtime-path mada-ttf "data/Mada/Mada-Regular.subset1.ttf")

(test-case
 "should open a font"
 (check-true (ttf-font? (open-font open-sans-ttf))))

(test-case 
 "should open fonts of different formats"
 (check-true (ttf-font? (open-font open-sans-ttf)))
 (check-true (ttf-font? (open-font source-sans-otf))))

(test-case
 "should open fonts lacking PostScript name"
 (define font (open-font mada-ttf))
 (check-false (font-postscript-name font)))

#;(test-case
 "should error when opening an invalid font asynchronously"
 (check-exn exn:fail? (λ () (open-font "nowhere"))))

