#lang at-exp s-exp pitfall/render

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
/F1 24 Tf
1 0 0 1 100 100 Tm
1 0 0 RG
[2] 0 d
0.75 g
2 Tr
(Hello) Tj
2 0 0 2 160 100 Tm
0 0 0 RG
0 g
(World) Tj
ET
"))


;; font
(co-io 6 0
       (make-co-dict
        'Type 'Font 'Subtype 'Type1 'Name 'F1 'BaseFont 'Helvetica))



(co-io 7 0
       (make-co-dict 'Type 'Annot
                     'Subtype 'Link
                     'Rect (co-array '(100 100 150 125))
                     'A (co-io-ref 8 0)))


(co-io 8 0
       (make-co-dict 'Type 'Action
                     'S 'URI
                     'URI (co-string "http://practicaltypography.com")))