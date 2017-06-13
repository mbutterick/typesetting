#lang fontkit/racket
(require restructure "tables.rkt")

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define TableEntry (make-object Struct
                     (dictify 'tag (+String 4)
                              'checkSum uint32be
                              'offset uint32be
                              'length uint32be)))

(define-subclass Struct (RDirectory)
  (define/override (process this-res stream)
    ;; in `restructure` `process` method, `res` is aliased as `this`
    (define new-tables-val (mhash))
    (for ([table (in-list (· this-res tables))])
      (hash-set! new-tables-val (string->symbol (· table tag)) table))
    (hash-set! this-res 'tables new-tables-val))

  (define/override (preEncode this-val stream)
    (report 'start-directory-preEncode)
    (define tables empty)
    (for ([(tag table) (in-hash (· this-val tables))])
      (when table
        (push-end! tables
                   (mhash
                    'tag tag
                    'checkSum 0
                    'offset #xdeadbeef ; todo
                    'length (let ([tag (hash-ref table-decoders tag (λ () (raise-argument-error 'directory:preEncode "valid table tag" tag)))])
                              (report* tag table (send tag size table)))))))
    (define numTables (length tables))
    (define searchRange (* (floor (log (/ numTables (log 2)))) 16))
    (define entrySelector (floor (/ searchRange (log 2))))
    (define rangeShift (- (* numTables 16) searchRange))
    
    (hash-set*! this-val
                'tag "true"
                'numTables numTables
                'tables tables
                'searchRange searchRange
                'entrySelector rangeShift
                'rangeShift rangeShift)
    (report 'end-directory-preEncode)))
      

(define Directory (make-object RDirectory
                    (dictify 'tag (+String 4)
                             'numTables uint16be
                             'searchRange uint16be
                             'entrySelector uint16be
                             'rangeShift uint16be
                             'tables (+Array TableEntry 'numTables))))

(define (directory-decode ip [options (mhash)])
  (send Directory decode (+DecodeStream (port->bytes ip))))


(test-module
 (define ip (open-input-file charter-path))
 (define decoded-dir (deserialize (read (open-input-file charter-directory-path))))
 (check-equal? (directory-decode ip) decoded-dir)
 (define es (+EncodeStream))
 ;(send Directory encode es decoded-dir)
 )