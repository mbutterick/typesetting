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
         (hash-set! new-tables-val (string->symbol (· table tag)) table))
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
                  0
                  ()
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
                    (loca h ! () (tag u . "loca") (offset . 38692) (checkSum . 2795817194) (length . 460))
                    (glyf h ! () (tag u . "glyf") (offset . 4620) (checkSum . 1143629849) (length . 34072))
                    (OS/2 h ! () (tag u . "OS/2") (offset . 360) (checkSum . 2351070438) (length . 96))
                    (hhea h ! () (tag u . "hhea") (offset . 292) (checkSum . 132056097) (length . 36))
                    (post h ! () (tag u . "post") (offset . 41520) (checkSum . 1670855689) (length . 514))
                    (|cvt | h ! () (tag u . "cvt ") (offset . 4592) (checkSum . 10290865) (length . 26))
                    (VDMX h ! () (tag u . "VDMX") (offset . 1372) (checkSum . 1887795202) (length . 1504))
                    (prep h ! () (tag u . "prep") (offset . 4512) (checkSum . 490862356) (length . 78))
                    (maxp h ! () (tag u . "maxp") (offset . 328) (checkSum . 50135594) (length . 32))
                    (hmtx h ! () (tag u . "hmtx") (offset . 456) (checkSum . 3982043058) (length . 916))
                    (cmap h ! () (tag u . "cmap") (offset . 2876) (checkSum . 1723761408) (length . 1262))
                    (name h ! () (tag u . "name") (offset . 39152) (checkSum . 2629707307) (length . 2367))
                    (head h ! () (tag u . "head") (offset . 236) (checkSum . 4281190895) (length . 54))
                    (fpgm h ! () (tag u . "fpgm") (offset . 4140) (checkSum . 106535991) (length . 371))))))))