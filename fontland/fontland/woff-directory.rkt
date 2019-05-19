#lang racket/base
(require xenomorph
         "tables.rkt"
         sugar/unstable/dict)
(provide woff-directory)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/WOFFDirectory.js
|#

(define woff-directory-entry
  (x:dict
   (dictify
    'tag (x:symbol #:length 4)
    'offset (x:pointer #:type uint32be
                       #:dest-type 'void
                       #:relative-to 'global)
    'compLength uint32be
    'length uint32be
    'origChecksum uint32be)))

(define (woff-directory-process dir)
  (define tables (make-hasheq))
  (for ([table (in-list (hash-ref dir 'tables))])
    (hash-set! tables (hash-ref table 'tag) table))
  (hash-set! dir 'tables tables)
  dir)

(define woff-directory
  (x:dict
   #:post-decode woff-directory-process
   (dictify
    'tag (x:symbol #:length 4) ;should be 'wOFF
    'flavor uint32be
    'length uint32be
    'numTables uint16be
    'reserved (x:reserved #:type uint16be)
    'totalSfntSize uint32be
    'majorVersion uint16be
    'minorVersion uint16be
    'metaOffset uint32be
    'metaLength uint32be
    'metaOrigLength uint32be
    'privOffset uint32be
    'privLength uint32be
    'tables (x:list #:type woff-directory-entry
                    #:length (λ (p) (hash-ref p 'numTables))))))

(module+ test
  (require rackunit "helper.rkt" racket/serialize racket/file racket/pretty)
  (define ip (open-input-file charter-woff-path))
  (define dir (decode woff-directory ip))
  (check-equal? (hash-ref dir 'tag) 'wOFF)
  (define offset (hash-ref (hash-ref (hash-ref dir 'tables) 'head) 'offset))
  (file-position ip offset)
  (define name-table (decode head ip))
  (check-equal? (hash-ref name-table 'magicNumber) #x5F0F3CF5)
  (check-equal? (hash-ref name-table 'unitsPerEm) 1000)
  (check-equal? (hash-ref name-table 'created) (date* 52 12 12 10 7 2013 3 190 #f 0 0 "UTC")))