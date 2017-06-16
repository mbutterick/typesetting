#lang fontkit/racket
(require restructure "tables.rkt")

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#


(define TableEntry (+Struct
                    (dictify 'tag (+String 4)
                             'checkSum uint32be
                             'offset uint32be
                             'length uint32be)))

(define (pad-to-32bit bstr)
  (define op (open-output-bytes))
  (write-bytes bstr op)
  (file-position op (* (ceiling (/ (file-position op) 4)) 4))
  (get-output-bytes op))

(test-module
 (check-equal? (pad-to-32bit #"") #"")
 (check-equal? (pad-to-32bit #"1") #"1\0\0\0")
 (check-equal? (pad-to-32bit #"12") #"12\0\0")
 (check-equal? (pad-to-32bit #"123") #"123\0")
 (check-equal? (pad-to-32bit #"1234") #"1234"))

;; for stupid tags like 'cvt '
(define (symbol-replace sym this that)
  (string->symbol (string-replace (if (string? sym) sym (symbol->string sym)) this that)))
(define (escape-tag tag) (symbol-replace tag " " "_"))
(define (unescape-tag tag) (symbol-replace tag "_" " "))

(define-subclass Struct (RDirectory)
  (define/override (process this-res stream)
    (define new-tables-val (mhash))
    (for ([table (in-list (· this-res tables))])
         (hash-set! new-tables-val (escape-tag (· table tag)) table))
    (hash-set! this-res 'tables new-tables-val))

  (define/override (preEncode this-val stream)
    (define preamble-length 12)
    (define table-header-size (+ preamble-length
                                 (* (length (hash-keys (· this-val tables))) (send TableEntry size))))

    (define-values (table-headers table-datas)
      (for/lists (ths tds)
                 ([(tag table) (in-hash (· this-val tables))])

                 (define table-data
                   (let ([es (+EncodeStream)])
                     (send (hash-ref table-codecs tag) encode es table)
                     (send es dump)))

                 (define table-header (mhash
                                       'tag (unescape-tag tag)
                                       'checkSum 0
                                       'offset (apply + (cons table-header-size (map bytes-length tds)))
                                       'length (bytes-length table-data)))

                 (define table-data-padded (pad-to-32bit table-data))
                 (values table-header table-data-padded)))


    (define numTables (length table-headers))
    (define searchRange (* (floor (log numTables 2)) 16))
    
    (hash-set*! this-val
                'tag "true"
                'numTables numTables
                'tables table-headers
                'searchRange searchRange
                'entrySelector (floor (/ searchRange (log 2)))
                'rangeShift (- (* numTables 16) searchRange)
                'data table-datas)))
      

(define Directory (+RDirectory
                   (dictify 'tag (+String 4)
                            'numTables uint16be
                            'searchRange uint16be
                            'entrySelector uint16be
                            'rangeShift uint16be
                            'tables (+Array TableEntry 'numTables)
                            
                            ;; we don't know what tables we might get
                            ;; so we represent as generic Buffer type,
                            ;; and convert the tables to bytes manually in preEncode
                            'data (+Array (+Buffer)))))

(define (directory-decode ip [options (mhash)])
  (send Directory decode (+DecodeStream (port->bytes ip))))

(define (file-directory-decode ps)
  (directory-decode (open-input-file ps)))

(test-module
 (define ip (open-input-file charter-path))
 (define decoded-dir (deserialize (read (open-input-file charter-directory-path))))
 (check-equal? (directory-decode ip) decoded-dir)
 
 )