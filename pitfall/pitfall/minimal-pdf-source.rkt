#lang racket/base
(require pitfall/struct pitfall/render)

(define (io1)
  (hasheq 'Pages "2 0 R" 'Type 'Catalog))

(define (io2)
  (hasheq 'Count 1 'Kids '("3 0 R") 'Type 'Pages 'MediaBox '(0 0 300 144)))

(define (io3)
  (hasheq 'Resources
          (hasheq 'Font
                  (hasheq 'F1 (hasheq 'Subtype 'Type1 'BaseFont 'Times-Roman
                                      'Type 'Font)))
          'Parent "2 0 R"
          'Contents "4 0 R"
          'Type 'Page))

(define (io4)
  ($stream (hasheq 'Length 55) #"  BT\n    /F1 18 Tf\n    0 0 Td\n    (Hello World) Tj\n  ET"))

(display (cosexpr->string (list io1 io2 io3 io4)))