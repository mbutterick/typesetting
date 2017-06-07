#lang pitfall/racket
(provide (all-defined-out))

(require binparser/object)



(define TableEntry (:seq ([tag (:make-string 4)]
                          [checkSum uint32be]
                          [offset uint32be]
                          [length uint32be])))

(define Directory (:seq ([tag hexbytes #:assert (curry equal? "00 01 00 00")]
                         [numTables uint16be #:assert ]
                         [searchRange uint16be]
                         [entrySelector uint16be]
                         [rangeShift uint16be]
                         [tables (:repeat numTables TableEntry)])))

(define (directory-decode ip [options (mhash)])
  (Directory ip))


(define ip (open-input-file "test/assets/Charter.ttf"))
(directory-decode ip (mhash '_startOffset 0))
(module+ test
  (require rackunit)
  (define ip (open-input-file "test/assets/Charter.ttf"))
  (check-equal?
   (directory-decode ip (mhash '_startOffset 0))
   '((tag . "00 01 00 00")
     (numTables . 14)
     (searchRange . 128)
     (entrySelector . 3)
     (rangeShift . 96)
     (tables
      ((tag . "OS/2") (checkSum . 2351070438) (offset . 360) (length . 96))
      ((tag . "VDMX") (checkSum . 1887795202) (offset . 1372) (length . 1504))
      ((tag . "cmap") (checkSum . 1723761408) (offset . 2876) (length . 1262))
      ((tag . "cvt ") (checkSum . 10290865) (offset . 4592) (length . 26))
      ((tag . "fpgm") (checkSum . 106535991) (offset . 4140) (length . 371))
      ((tag . "glyf") (checkSum . 1143629849) (offset . 4620) (length . 34072))
      ((tag . "head") (checkSum . 4281190895) (offset . 236) (length . 54))
      ((tag . "hhea") (checkSum . 132056097) (offset . 292) (length . 36))
      ((tag . "hmtx") (checkSum . 3982043058) (offset . 456) (length . 916))
      ((tag . "loca") (checkSum . 2795817194) (offset . 38692) (length . 460))
      ((tag . "maxp") (checkSum . 50135594) (offset . 328) (length . 32))
      ((tag . "name") (checkSum . 2629707307) (offset . 39152) (length . 2367))
      ((tag . "post") (checkSum . 1670855689) (offset . 41520) (length . 514))
      ((tag . "prep") (checkSum . 490862356) (offset . 4512) (length . 78))))))