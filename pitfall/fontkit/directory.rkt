#lang fontkit/racket
(require xenomorph "tables.rkt" describe)

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define TableEntry (+Struct
                    (dictify 'tag (+Symbol 4)
                             'checkSum uint32be
                             'offset (+Pointer uint32be 'void (mhash 'type 'global))
                             'length uint32be)))

;; for stupid tags like 'cvt '
(define (symbol-replace sym this that)
  (string->symbol (string-replace (if (string? sym) sym (symbol->string sym)) this that)))
(define (escape-tag tag) (symbol-replace tag " " "_"))
(define (unescape-tag tag) (symbol-replace tag "_" " "))

(define-subclass Struct (RDirectory)
  (define/augride (post-decode this-res stream ctx)
    (define new-tables-val (mhash))
    (for ([table (in-list (· this-res tables))])
         (hash-set! new-tables-val (escape-tag (· table tag)) table))
    (dict-set! this-res 'tables new-tables-val)
    this-res)

  (define/augride (pre-encode this-val port)
    (define tables (for/list ([(tag table) (in-hash (· this-val tables))])
                             (define table-codec (hash-ref table-codecs tag))
                             (mhash 'tag (unescape-tag tag)
                                    'checkSum 0
                                    'offset (+VoidPointer table-codec table)
                                    'length (send table-codec size table))))

    (define numTables (length tables))
    (define searchRange (* (floor (log numTables 2)) 16))
    
    (hash-set*! this-val
                'tag 'true
                'numTables numTables
                'tables tables
                'searchRange searchRange
                'entrySelector (floor (/ searchRange (log 2)))
                'rangeShift (- (* numTables 16) searchRange))

    this-val))

(define Directory (+RDirectory (dictify 'tag (+Symbol 4)
                                        'numTables uint16be
                                        'searchRange uint16be
                                        'entrySelector uint16be
                                        'rangeShift uint16be
                                        'tables (+Array TableEntry 'numTables))))
                            

(define (directory-decode ip [options (mhash)])
  (send Directory decode ip))

(define (file-directory-decode ps)
  (directory-decode (open-input-file ps)))

#;(test-module
   (define ip (open-input-file charter-path))
   (define decoded-dir (deserialize (read (open-input-file charter-directory-path))))
   (check-equal? (directory-decode ip) decoded-dir))