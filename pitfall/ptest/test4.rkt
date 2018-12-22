#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send* doc
     [font "Courier-Bold"]
     [font-size 10]
     [text "Hello"]
     [translate -30 30]
     [font "Courier-BoldOblique"]
     [font-size 11]
     [text "Hello"]
     [translate -30 30]
     [font "Courier-Oblique"]
     [font-size 12]
     [text "Hello"]
     [translate -30 30]
     [font "Courier"]
     [font-size 14]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-Bold"]
     [font-size 16]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-BoldOblique"]
     [font-size 18]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica-Oblique"]
     [font-size 20]
     [text "Hello"]
     [translate -30 30]
     [font "Helvetica"]
     [font-size 22]
     [text "Hello"]
     [translate -30 30]
     [font "Symbol"]
     [font-size 24]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Bold"]
     [font-size 26]
     [text "Hello"]
     [translate -30 30]
     [font "Times-BoldItalic"]
     [font-size 28]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Italic"]
     [font-size 30]
     [text "Hello"]
     [translate -30 30]
     [font "Times-Roman"]
     [font-size 32]
     [text "Hello"]
     [translate -30 30]
     [font "ZapfDingbats"]
     [font-size 34]
     [text "Hello"]))

(define-runtime-path this "test4rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test4crkt.pdf")
(make-doc that #t proc)

