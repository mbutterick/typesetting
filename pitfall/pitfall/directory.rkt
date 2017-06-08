#lang pitfall/racket
(require restructure)

(provide (all-defined-out))


(define TableEntry (make-object RStruct
                     (list (cons 'tag (make-object RString 4))
                           (cons 'checkSum uint32be)
                           (cons 'offset uint32be)
                           (cons 'length uint32be))))

(define Directory (make-object RStruct
                    (list (cons 'tag (make-object RString 4))
                          (cons 'numTables uint16be)
                          (cons 'searchRange uint16be)
                          (cons 'entrySelector uint16be)
                          (cons 'rangeShift uint16be)
                          ;; todo next: derive the `14` from 'numTables
                          (cons 'tables (make-object RArray TableEntry 14)))))

(module+ test
  (require rackunit)
  (define ip (open-input-file "test/assets/Charter.ttf"))
  (define is (make-object RDecodeStream ip))
  (check-equal?
   (send Directory decode is)
   (make-hasheq
    (list (cons 'tables
                (list (make-hasheq '((length . 96) (checkSum . 2351070438) (offset . 360) (tag . "OS/2")))
                      (make-hasheq '((length . 1504) (checkSum . 1887795202) (offset . 1372) (tag . "VDMX")))
                      (make-hasheq '((length . 1262) (checkSum . 1723761408) (offset . 2876) (tag . "cmap")))
                      (make-hasheq '((length . 26) (checkSum . 10290865) (offset . 4592) (tag . "cvt ")))
                      (make-hasheq '((length . 371) (checkSum . 106535991) (offset . 4140) (tag . "fpgm")))
                      (make-hasheq '((length . 34072) (checkSum . 1143629849) (offset . 4620) (tag . "glyf")))
                      (make-hasheq '((length . 54) (checkSum . 4281190895) (offset . 236) (tag . "head")))
                      (make-hasheq '((length . 36) (checkSum . 132056097) (offset . 292) (tag . "hhea")))
                      (make-hasheq '((length . 916) (checkSum . 3982043058) (offset . 456) (tag . "hmtx")))
                      (make-hasheq '((length . 460) (checkSum . 2795817194) (offset . 38692) (tag . "loca")))
                      (make-hasheq '((length . 32) (checkSum . 50135594) (offset . 328) (tag . "maxp")))
                      (make-hasheq '((length . 2367) (checkSum . 2629707307) (offset . 39152) (tag . "name")))
                      (make-hasheq '((length . 514) (checkSum . 1670855689) (offset . 41520) (tag . "post")))
                      (make-hasheq '((length . 78) (checkSum . 490862356) (offset . 4512) (tag . "prep")))))
          (cons 'entrySelector 3)
          (cons 'numTables 14)
          (cons 'searchRange 128)
          (cons 'rangeShift 96)
          (cons 'tag "\u0000\u0001\u0000\u0000")))))