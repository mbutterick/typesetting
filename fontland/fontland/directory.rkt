#lang racket/base
(require xenomorph
         "tables.rkt"
         racket/dict
         sugar/unstable/dict
         racket/string)

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define table-entry (x:struct
                    'tag (x:symbol #:length 4)
                    'checkSum uint32be
                    'offset (x:pointer #:offset-type uint32be
                                       #:type 'void
                                       #:relative-to 'global)
                    'length uint32be))

;; for stupid tags like 'cvt '
(define (symbol-replace sym this that)
  (string->symbol (string-replace (if (string? sym) sym (symbol->string sym)) this that)))
(define (escape-tag tag) (symbol-replace tag " " "_"))
(define (unescape-tag tag) (symbol-replace tag "_" " "))

(define (directory-post-decode this-res)
  (define new-tables-val (mhash))
  (for ([table (in-list (hash-ref this-res 'tables))])
    (hash-set! new-tables-val (escape-tag (hash-ref table 'tag)) table))
  (dict-set! this-res 'tables new-tables-val)
  this-res)

(define (directory-pre-encode this-val)
  (define tables (for/list ([(tag table) (in-hash (hash-ref this-val 'tables))])
                   (define table-codec (hash-ref table-codecs tag))
                   (mhash 'tag (unescape-tag tag)
                          'checkSum 0
                          'offset (x:void-pointer table-codec table)
                          'length (size table-codec table))))
  (define numTables (length tables))
  (define searchRange (* (floor (log numTables 2)) 16))
  (hash-set*! this-val
              'tag 'true
              'numTables numTables
              'tables tables
              'searchRange searchRange
              'entrySelector (floor (/ searchRange (log 2)))
              'rangeShift (- (* numTables 16) searchRange))
  this-val)

(define Directory (x:struct #:pre-encode directory-pre-encode
                            #:post-decode directory-post-decode
                            'tag (x:symbol #:length 4)
                            'numTables uint16be
                            'searchRange uint16be
                            'entrySelector uint16be
                            'rangeShift uint16be
                            'tables (x:array #:type table-entry #:length 'numTables)))

(define (directory-decode ip [options (mhash)])
  (decode Directory ip))

(define (file-directory-decode ps)
  (directory-decode (open-input-file ps)))

#;(test-module
   (define ip (open-input-file charter-path))
   (define decoded-dir (deserialize (read (open-input-file charter-directory-path))))
   (check-equal? (directory-decode ip) decoded-dir))