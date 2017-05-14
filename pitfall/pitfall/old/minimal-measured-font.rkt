#lang at-exp br
(require pitfall/render)

(render-args

 ;; catalog object
 (co-io 1 0 (co-catalog #:pages (co-io-ref 2 0)))

 ;; pages
 (co-io 2 0 (co-pages #:kids (list (co-io-ref 3 0))
                      #:count 1))
 ;; page
 (co-io 3 0 (co-page #:parent (co-io-ref 2 0)
                     #:mediabox '(0 0 400 400)
                     #:resources (co-io-ref 4 0)
                     #:contents (co-io-ref 5 0)
                    
                     ; the value of annots must be an array
                     #:annots (co-array (list (co-io-ref 7 0)))
                     ))
 #;#:annots (co-io-ref 7 0)

 ;; resources
 (co-io 4 0
        (make-co-dict
         'ProcSet (co-array '(PDF Text))
         'Font (make-co-dict 'F1 (co-io-ref 6 0))))

 ;; contents
 (co-io 5 0
        (make-co-stream
         #"
BT
/F1 48 Tf
(Hello World 1-1!) Tj
ET
"))


 ;; font
 (let ([last-char 255])
   (co-io 6 0
          (make-co-dict
           'Type 'Font
           'Subtype 'Type1
           'Name 'F1
           'BaseFont 'Helvetica
           'FontDescriptor (co-io-ref 9 0)
           'FirstChar 0
           'LastChar last-char
           ;; widths can only hold 256 apparently?
           'Widths (report (co-array
                            (for/list ([i (in-range (add1 last-char))])
                              ;; 29 is magic number: this is offset betwee
                              ;; ascii value and character index
                              ;; so A (ascii 65) lives at index 36
                              (define ascii-index-offset 29)
                              (if (<= ascii-index-offset i)
                                  (measure-char-idx "fira.otf" (- i ascii-index-offset))
                                  200))))
           'Encoding 'MacRomanEncoding)))

 (co-io 7 0
        (make-co-dict 'Type 'Annot
                      'Subtype 'Link
                      'Rect (co-array '(100 100 150 125))
                      'A (co-io-ref 8 0)))


 (co-io 8 0
        (make-co-dict 'Type 'Action
                      'S 'URI
                      'URI (co-string "http://practicaltypography.com")))

 (co-io 9 0
        (make-co-dict
         'Type 'FontDescriptor
         'FontName 'FiraSansOTMedium
         'FontFile3 (co-io-ref 10 0)
         'Flags 4
         'FontBBox (co-array '(-177 -269 1123 866))
         'MissingWidth 255
         'StemV 105
         'StemH 45
         'CapHeight 660
         'XHeight 394
         'Ascent 720
         'Descent -270
         'Leading 83
         'MaxWidth 1212
         'AvgWidth 478
         'ItalicAngle 0))

 (co-io 10 0 (make-font-co-stream "fira.otf")))
