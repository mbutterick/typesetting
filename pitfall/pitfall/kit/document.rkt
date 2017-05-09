#lang br
(require racket/draw)
(provide PDFDocument)

(define PDFDocument
  (class pdf-dc%
    (init output-file)
    (super-new [interactive #f]	 
               [parent #f]	 
               [use-paper-bbox #f]	 
               [as-eps #f]	 
               [width #f]	 
               [height #f]	 
               [output (open-output-file output-file #:exists 'replace)])

    (define (end-doc)
      (display 'whee))
    (override end-doc)
    
    (define (end) (end-doc))
    (public end)))
