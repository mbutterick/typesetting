#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send* doc
     [font "Courier-Bold"]
     [fontSize 10]
     [text "Hello"]
     [translate -30 30]
     [font "Courier-BoldOblique"]
     [fontSize 11]
     [text "Hello"]
     [translate -30 30]
     [font "Courier-Oblique"]
     [fontSize 12]
     [text "Hello"]
     [translate -30 30]
     [font "Courier"]
     [fontSize 14]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-Bold"]
     [fontSize 16]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-BoldOblique"]
     [fontSize 18]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-Oblique"]
     [fontSize 20]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica"]
     [fontSize 22]
     [text "Hello"]
     [translate -30 30]
     [font "Symbol"]
     [fontSize 24]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Bold"]
     [fontSize 26]
     [text "Hello"]
     [translate -30 30]
     [font "Times-BoldItalic"]
     [fontSize 28]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Italic"]
     [fontSize 30]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Roman"]
     [fontSize 32]
     [text "Hello"]
     [translate -30 30]
     [font "ZapfDingbats"]
     [fontSize 34]
     [text "Hello"]))

(define-runtime-path this "test4rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test4crkt.pdf")
(make-doc that #t proc)

