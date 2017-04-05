#lang at-exp s-exp pitfall/render

;; catalog object
(co-io 1 0 (co-catalog #:pages (co-io-ref 2 0)))

;; pages
(co-io 2 0 (co-pages #:kids (list (co-io-ref 3 0))
                     #:count 1))
;; page
(co-io 3 0 (co-page #:parent (co-io-ref 2 0)
                    #:mediabox '(0 0 612 792)
                    #:resources (co-io-ref 4 0)
                    #:contents (co-io-ref 5 0)
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
        #"BT
/F1 24 Tf
1 0 0 1 260 600 Tm
(Hello World)Tj
ET"))


;; font
(co-io 6 0
       (make-co-dict
        'Type 'Font 'Subtype 'Type1 'Name 'F1 'BaseFont 'Helvetica))

;; annots
#|
(co-io 7 0
       (co-array (list (co-io-ref 8 0)))

(co-io 8 0
       (co-dict
        (hasheq
         'Rect
         (co-array '(260 596.0184 404.048 613.331))
         'Subtype
         'StrikeOut
         'Contents
         ""
         'F
         4
         'M
         "D:20170405003853Z00'00'"
         'Type
         'Annot
         'C
         (co-array '(0.8945577 0.05464891 0.07036456))
         'AP
         (co-io-ref 11 0)
         'QuadPoints
         (co-array '(260 613.331 404.048 613.331 260 596.0184 404.048 596.0184)))))
|#