#lang pitfall/racket
(require restructure)

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define TableEntry (make-object RStruct
                     (dictify 'tag (make-object RString 4)
                              'checkSum uint32be
                              'offset uint32be
                              'length uint32be)))

(define-subclass RStruct (RDirectory)
  (define/public (process)
    'boom))

(define Directory (make-object RDirectory
                    (dictify 'tag (make-object RString 4)
                             'numTables uint16be
                             'searchRange uint16be
                             'entrySelector uint16be
                             'rangeShift uint16be
                             'tables (make-object RArray TableEntry 'numTables))))

(define (directory-decode ip [options (mhash)])
  (define is (make-object RDecodeStream ip))
  (send Directory decode is))

(module+ test
  (require rackunit)
  (define ip (open-input-file "test/assets/Charter.ttf"))
  (check-equal?
   (directory-decode ip)
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