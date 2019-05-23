#lang racket/base
(require xenomorph
         "tables.rkt"
         racket/dict
         racket/class
         racket/match
         sugar/unstable/dict
         racket/string)

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define table-entry (x:struct
                    'tag (x:symbol #:length 4)
                    'checkSum uint32be
                    'offset (x:pointer #:type uint32be
                                       #:dest-type 'void
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
  (define tables (for/list ([tag-table-pair (in-list (hash-ref this-val 'tables))])
                   (match-define (cons tag table) tag-table-pair)
                   (define table-codec (hash-ref table-codecs tag))
                   (mhash 'tag (unescape-tag tag)
                          'checkSum 0
                          'offset (x:void-pointer table-codec table)
                          'length (send table-codec x:size table))))
  (define numTables (length tables))
  ;; patch from https://github.com/foliojs/fontkit/pull/178
  (define max-exponent-for-2 (floor (log numTables 2)))
  (define searchRange (* (expt 2 max-exponent-for-2) 16))
  (hash-set*! this-val
              'tag 'true
              'numTables numTables
              'tables tables
              'searchRange searchRange
              'entrySelector max-exponent-for-2
              'rangeShift (- (* numTables 16) searchRange))
  this-val)

(define Directory (x:struct #:pre-encode directory-pre-encode
                            #:post-decode directory-post-decode
                            'tag (x:symbol #:length 4)
                            'numTables uint16be
                            'searchRange uint16be
                            'entrySelector uint16be
                            'rangeShift uint16be
                            'tables (x:array #:type table-entry #:length (λ (p) (hash-ref p 'numTables)))))

(define (directory-decode ip [options (mhash)])
  (decode Directory ip))

(define (file-directory-decode ps)
  (directory-decode (open-input-file ps)))

#;(module+ test
  (require rackunit "helper.rkt" racket/serialize racket/file racket/pretty)
   (define ip (open-input-file fira-otf-path))
  (define dir  (serialize (directory-decode ip)))
  (pretty-write dir)
   (with-output-to-file    "assets/fira-otf-directory.rktd"
     (λ () (pretty-write dir)) #:exists 'replace))