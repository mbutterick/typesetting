#lang at-exp br
(require pitfall/render)

(define cosh (make-co-hash))

;; catalog object
(+co-hash cosh 1 (co-catalog #:pages (make-co-io-ref 2)))

;; pages
(+co-hash cosh 2 (co-pages #:kids (list (make-co-io-ref 3))
                           #:count 1))
;; page
(+co-hash cosh 3 (co-page #:parent (make-co-io-ref 2)
                          #:mediabox '(0 0 400 400)
                          #:resources (make-co-io-ref 4)
                          #:contents (make-co-io-ref 5)
                    
                          ; the value of annots must be an array
                          #:annots (co-array (list (make-co-io-ref 7)))
                          ))
#;#:annots #;(make-co-io-ref 7)

;; resources
(+co-hash cosh 4
          (make-co-dict
           'ProcSet (co-array '(PDF Text))
           'Font (make-co-dict 'F1 (make-co-io-ref 6)
                               'F2 (make-co-io-ref 9)
                               'F3 (make-co-io-ref 12))))



(define (pdf-char-encode c)
  (format "\\~a" (number->string (char->integer c) 8)))

(define (typeset-text str size font-path)
  (string->bytes/latin-1
   (string-append*
    (for/list ([c (in-string str)])
      (format "(~a) Tj ~a 0 Td"
              (pdf-char-encode c)
              (/ (* size (measure-char font-path c)) 1000.0))))))



;; Charter font
(define charter-font-path "charter.otf")
(+co-hash cosh 6
          (make-co-dict
           'Type 'Font
           'Subtype 'Type1
           'Name 'F1
           'FontDescriptor (make-co-dict
                            'Type 'FontDescriptor
                            'FontFile3 (make-co-io-ref 8)
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
                            'ItalicAngle 0)
           'Encoding (make-co-io-ref 25)))

(require "glyph-list.rkt")
(+co-hash cosh 25
          (make-co-dict
           'Type 'Encoding
           'Differences (co-array (flatten glyph-list))))

(+co-hash cosh 8 (make-font-co-stream charter-font-path))

;; Miso font
(define miso-font-path "miso.otf")
(+co-hash cosh 9
          (make-co-dict
           'Type 'Font
           'Subtype 'Type1
           'Name 'F2
           'FontDescriptor (make-co-dict
                            'Type 'FontDescriptor
                            'FontFile3 (make-co-io-ref 10)
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
                            'ItalicAngle 0)
           'Encoding (make-co-io-ref 25)))

(+co-hash cosh 10 (make-font-co-stream miso-font-path))

;; fira font
(define fira-font-path "fira.otf")
(+co-hash cosh 12
          (make-co-dict
           'Type 'Font
           'Subtype 'Type1
           'Name 'F3
           'FontDescriptor (make-co-dict
                            'Type 'FontDescriptor
                            'FontFile3 (make-co-io-ref 13)
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
                            'ItalicAngle 0)
           'Encoding (make-co-io-ref 25)))

(+co-hash cosh 13 (make-font-co-stream fira-font-path))


;; contents
(+co-hash cosh 5
          (make-co-stream
           (bytes-append
            #"BT /F1 36 Tf 50 50 Td"
            (typeset-text "Hel’o Wørld 4-1" 36 charter-font-path)
            #"/F2 36 Tf -250 50 Td"
            (typeset-text "Ençhîládà 10" 36 miso-font-path)
            #"/F3 36 Tf -150 50 Td"
            (typeset-text "Bõîng Mé 5" 36 fira-font-path)
            #"ET")))

(render cosh)
