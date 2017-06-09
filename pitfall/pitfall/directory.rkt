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
  (define/override (process res stream)
    (define new-tables-val (mhash))
    (for ([table (in-list (· res tables))])
         (hash-set! new-tables-val (· table tag) table))
    (hash-set! res 'tables new-tables-val)))

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
  (require rackunit racket/serialize)
  (define ip (open-input-file "test/assets/Charter.ttf"))
  (check-equal?
   (directory-decode ip)
   (deserialize '((3)
                  0
                  ()
                  14
                  ((u . "head")
                   (u . "prep")
                   (u . "fpgm")
                   (u . "hmtx")
                   (u . "hhea")
                   (u . "maxp")
                   (u . "VDMX")
                   (u . "loca")
                   (u . "name")
                   (u . "cvt ")
                   (u . "OS/2")
                   (u . "post")
                   (u . "glyf")
                   (u . "cmap"))
                  ()
                  (h
                   !
                   ()
                   (tag u . "\u0000\u0001\u0000\u0000")
                   (rangeShift . 96)
                   (searchRange . 128)
                   (numTables . 14)
                   (entrySelector . 3)
                   (tables
                    h
                    !
                    (equal)
                    ((? . 0) h ! () (tag ? . 0) (offset . 236) (checkSum . 4281190895) (length . 54))
                    ((? . 1) h ! () (tag ? . 1) (offset . 4512) (checkSum . 490862356) (length . 78))
                    ((? . 2) h ! () (tag ? . 2) (offset . 4140) (checkSum . 106535991) (length . 371))
                    ((? . 3) h ! () (tag ? . 3) (offset . 456) (checkSum . 3982043058) (length . 916))
                    ((? . 4) h ! () (tag ? . 4) (offset . 292) (checkSum . 132056097) (length . 36))
                    ((? . 5) h ! () (tag ? . 5) (offset . 328) (checkSum . 50135594) (length . 32))
                    ((? . 6) h ! () (tag ? . 6) (offset . 1372) (checkSum . 1887795202) (length . 1504))
                    ((? . 7) h ! () (tag ? . 7) (offset . 38692) (checkSum . 2795817194) (length . 460))
                    ((? . 8) h ! () (tag ? . 8) (offset . 39152) (checkSum . 2629707307) (length . 2367))
                    ((? . 9) h ! () (tag ? . 9) (offset . 4592) (checkSum . 10290865) (length . 26))
                    ((? . 10) h ! () (tag ? . 10) (offset . 360) (checkSum . 2351070438) (length . 96))
                    ((? . 11) h ! () (tag ? . 11) (offset . 41520) (checkSum . 1670855689) (length . 514))
                    ((? . 12) h ! () (tag ? . 12) (offset . 4620) (checkSum . 1143629849) (length . 34072))
                    ((? . 13) h ! () (tag ? . 13) (offset . 2876) (checkSum . 1723761408) (length . 1262))))))))