#lang debug racket/base
(require xenomorph
         "tables.rkt"
         "directory.rkt"
         racket/class
         racket/match
         racket/port
         sugar/unstable/dict)
(provide woff2-directory)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/WOFF2Directory.js
|#

(define uint32max (sub1 (expt 2 32)))
(define (fits-in-uint32? val) (<= val uint32max))
(define (times128 x) (arithmetic-shift x 8))
     
(define Base128%
  (class x:base%
    (super-new)
    (define/augment (x:decode port . _)
      ;; https://www.w3.org/TR/WOFF2/
      (when (equal? #x80 (peek-byte port))
        (error 'base128-no-leading-zero))
      (for/fold ([res 0])
                ([b (in-input-port-bytes port)]
                 [count (in-naturals 1)]
                 #:break (bitwise-bit-set? b 7))
        (when (> count 5)
          (error 'base128-longer-than-5-bytes))
        (let ([res (+ (times128 res) (bitwise-and b 127))])
          (unless (fits-in-uint32? res)
            (error 'base128-overflow))
          res)))
                
    (define/augment (x:encode val . _)
      (error 'Base128-encode-unimplemented))))

(define Base128 (new Base128%))

(module+ test
  (check-equal? (decode Base128 (bytes #x3f)) 63)
  (check-exn exn:fail? (λ () (decode Base128 (bytes #x80)))) ; leading zero
  (check-exn exn:fail? (λ () (decode Base128 (bytes #x3f #x3f #x3f #x3f #x3f #x3f)))) ; six bytes
  (check-exn exn:fail? (λ () (decode Base128 (bytes 127 127 127 127 127 128)))) ; overflow
  (check-equal? (decode Base128 (bytes #x3f 128 #x3f)) 63))

(define known-tags '(cmap head hhea hmtx maxp name OS/2 post |cvt | fpgm glyf loca prep |CFF | VORG EBDT EBLC gasp hdmx kern LTSH PCLT VDMX vhea vmtx BASE GDEF GPOS GSUB EBSC JSTF MATH CBDT CBLC COLR CPAL |SVG | sbix acnt avar bdat bloc bsln cvar fdsc feat fmtx fvar gvar hsty just lcar mort morx opbd prop trak Zapf Silf Glat Gloc Feat Sill))

(define woff2-directory-entry
  (x:dict
   (dictify
    'tag (x:symbol #:length 4)
    'offset (x:pointer #:type uint32be
                       #:dest-type 'void
                       #:relative-to 'global)
    'compLength uint32be
    'length uint32be
    'origChecksum uint32be)))

(define woff2-directory
  (x:dict
   #:post-decode directory-post-decode
   (dictify
    'tag (x:symbol #:length 4) ;should be 'wOF2
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
    'tables (x:list #:type woff2-directory-entry
                    #:length (λ (p) (hash-ref p 'numTables))))))

(module+ test
  (require rackunit "helper.rkt" racket/serialize racket/file racket/pretty)
  (define ip (open-input-file charter-woff-path))
  (define dir (decode woff2-directory ip))
  (check-equal? (hash-ref dir 'tag) 'wOFF)
  (define offset (hash-ref (hash-ref (hash-ref dir 'tables) 'head) 'offset))
  (file-position ip offset)
  (define name-table (decode head ip))
  (check-equal? (hash-ref name-table 'magicNumber) #x5F0F3CF5)
  (check-equal? (hash-ref name-table 'unitsPerEm) 1000)
  (check-equal? (hash-ref name-table 'created) (date* 52 12 12 10 7 2013 3 190 #f 0 0 "UTC")))