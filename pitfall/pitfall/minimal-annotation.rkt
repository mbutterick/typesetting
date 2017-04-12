#lang at-exp s-exp pitfall/render

;; catalog object
(co-io 1 0 (co-catalog #:pages (co-io-ref 2 0)))

;; pages
(co-io 2 0 (co-pages #:kids (list (co-io-ref 3 0))
                     #:count 1))
;; page
(co-io 3 0 (co-page #:parent (co-io-ref 2 0)
                    #:mediabox '(0 0 400 400)
                    #:contents (co-io-ref 5 0)
                    ; the value of annots must be an array
                    #:annots (co-array (list (co-io-ref 7 0)))))


;; contents
(co-io 5 0
       (make-co-stream
        #"
BT
0.9 g
100 100 100 100 re
F
ET
"))

(co-io 7 0
       (make-co-dict 'Type 'Annot
                     'Subtype 'Link
                     ;; rect is left bottom right top in page coordinates
                     'Rect (co-array '(125 125 175 175))
                     'A (co-io-ref 8 0)))


(co-io 8 0
       (make-co-dict 'Type 'Action
                     'S 'URI
                     'URI (co-string "http://practicaltypography.com")))