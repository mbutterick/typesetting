#lang racket/base
(require racket/class)
(provide PDFPage)


(define PDFPage
  (class object%
    (super-new)
    (init-field document
                [options (make-hasheq)])
    (field [size (hash-ref options 'size "letter")])
    (field [layout (hash-ref options 'layout "portrait")])

    ;; process margins
    (field [margins
            (let ([margin-value (hash-ref options 'margin #f)])
              (if (number? margin-value)
                  (hasheq 'top margin-value
                          'left margin-value
                          'bottom margin-value
                          'right margin-value)
                  ;; default to 1 inch margins
                  (hash-ref options 'margins DEFAULT_MARGINS)))])

    ;; calculate page dimensions
    (define dimensions (if (list? size)
                           size
                           (hash-ref SIZES (string-upcase size))))
    (field [width (list-ref dimensions (if (equal? layout "portrait") 0 1))])
    (field [height (list-ref dimensions (if (equal? layout "portrait") 1 0))])

    (field [content (send document ref)])

    ;; Initialize the Font, XObject, and ExtGState dictionaries
    (field [resources (send document ref (make-hash (list (cons 'ProcSet '("PDF" "Text" "ImageB" "ImageC" "ImageI")))))])

    ;; Lazily create these dictionaries
    (define/public (fonts)
      (hash-ref! (get-field data resources) 'Font (make-hash)))

    (define/public (xobjects)
      (hash-ref! (get-field data resources) 'XObject (make-hash)))

    (define/public (ext_gstates)
      (hash-ref! (get-field data resources) 'ExtGState (make-hash)))

    (define/public (patterns)
      (hash-ref! (get-field data resources) 'Pattern (make-hash)))

    (define/public (annotations)
      (hash-ref! (get-field data resources) 'Annots null))
          
    ;; The page dictionary
    (field [dictionary
            (send document ref
                             (make-hash (list
                                         (cons 'Type "Page")
                                         (cons 'Parent (hash-ref (get-field data (get-field _root document)) 'Pages))
                                         (cons 'MediaBox (list 0 0 width height))
                                         (cons 'Contents content)
                                         (cons 'Resources resources))))])

    
    
    ))


(define DEFAULT_MARGINS 
  (hasheq 'top 72
          'left 72
          'bottom 72
          'right 72))

(define SIZES
  (hash
   "4A0" '(4767.87 6740.79)
   "2A0" '(3370.39 4767.87)
   "A0" '(2383.94 3370.39)
   "A1" '(1683.78 2383.94)
   "A2" '(1190.55 1683.78)
   "A3" '(841.89 1190.55)
   "A4" '(595.28 841.89)
   "A5" '(419.53 595.28)
   "A6" '(297.64 419.53)
   "A7" '(209.76 297.64)
   "A8" '(147.40 209.76)
   "A9" '(104.88 147.40)
   "A10" '(73.70 104.88)
   "B0" '(2834.65 4008.19)
   "B1" '(2004.09 2834.65)
   "B2" '(1417.32 2004.09)
   "B3" '(1000.63 1417.32)
   "B4" '(708.66 1000.63)
   "B5" '(498.90 708.66)
   "B6" '(354.33 498.90)
   "B7" '(249.45 354.33)
   "B8" '(175.75 249.45)
   "B9" '(124.72 175.75)
   "B10" '(87.87 124.72)
   "C0" '(2599.37 3676.54)
   "C1" '(1836.85 2599.37)
   "C2" '(1298.27 1836.85)
   "C3" '(918.43 1298.27)
   "C4" '(649.13 918.43)
   "C5" '(459.21 649.13)
   "C6" '(323.15 459.21)
   "C7" '(229.61 323.15)
   "C8" '(161.57 229.61)
   "C9" '(113.39 161.57)
   "C10" '(79.37 113.39)
   "RA0" '(2437.80 3458.27)
   "RA1" '(1729.13 2437.80)
   "RA2" '(1218.90 1729.13)
   "RA3" '(864.57 1218.90)
   "RA4" '(609.45 864.57)
   "SRA0" '(2551.18 3628.35)
   "SRA1" '(1814.17 2551.18)
   "SRA2" '(1275.59 1814.17)
   "SRA3" '(907.09 1275.59)
   "SRA4" '(637.80 907.09)
   "EXECUTIVE" '(521.86 756.00)
   "FOLIO" '(612.00 936.00)
   "LEGAL" '(612.00 1008.00)
   "LETTER" '(612.00 792.00)
   "TABLOID" '(792.00 1224.00)))
