#lang s-exp pitfall/render

(co-version 1.1)
(co-comment "%¥±ë")
(co-io
 1
 0
 (co-dict (hasheq 'Pages (co-io-ref 2 0) 'Type 'Catalog)))
(co-io
 2
 0
 (co-dict
  (hasheq
   'Count
   1
   'Kids
   (co-array (list (co-io-ref 3 0)))
   'Type
   'Pages
   'MediaBox
   (co-array '(0 0 300 144)))))
(co-io
 3
 0
 (co-dict
  (hasheq
   'Resources
   (co-dict
    (hasheq
     'Font
     (co-dict
      (hasheq
       'F1
       (co-dict
        '#hasheq((Subtype . Type1)
                 (BaseFont . Times-Roman)
                 (Type . Font)))))))
   'Parent
   (co-io-ref 2 0)
   'Contents
   (co-io-ref 4 0)
   'Type
   'Page)))
(co-io
 4
 0
 (co-stream
  (co-dict '#hasheq((Length . 55)))
  #"  BT\n    /F1 18 Tf\n    0 0 Td\n    (Hello World) Tj\n  ET"))