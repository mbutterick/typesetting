#lang fontkit/racket
(require restructure "tables.rkt")

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/tables/directory.js
|#

(define-subclass Struct (RTableEntry)
  (define/override (preEncode this-val stream)
    (when (eq? (hash-ref this-val 'tag) 'cvt)
      (hash-set! this-val 'tag '|cvt |))))

(define TableEntry (+Struct
                    (dictify 'tag (+String 4)
                             'checkSum uint32be
                             'offset uint32be
                             'length uint32be)))

(define (pad-to-32bit bstr)
  (define mod (modulo (bytes-length bstr) 4))
  (if (positive? mod)
      (bytes-append bstr (make-bytes (- 4 mod) 0))
      bstr))

(define (symbol-replace sym this that)
  (string->symbol (string-replace (symbol->string sym) this that)))

(define (escape-tag tag)
  (symbol-replace (if (string? tag) (string->symbol tag) tag) " " "_"))

(define (unescape-tag tag)
  (symbol-replace (if (string? tag) (string->symbol tag) tag) "_" " "))

(define-subclass Struct (RDirectory)
  (define/override (process this-res stream)
    ;; in `restructure` `process` method, `res` is aliased as `this`
    (define new-tables-val (mhash))
    (for ([table (in-list (· this-res tables))])
         (hash-set! new-tables-val (escape-tag (· table tag)) table))
    (hash-set! this-res 'tables new-tables-val))

  (define/override (preEncode this-val stream)
    
    (define offset-ks (mhash))
    (define table-header-hash (mhash))
    (for ([(tag table) (in-hash (· this-val tables))]
          [i (in-naturals)]
          #:unless (hash-has-key? table-header-hash i))
         (hash-set! table-header-hash i
                    (let/cc k
                      (hash-set! offset-ks i k)
                      (mhash
                       'tag (unescape-tag tag)))))

    (define table-headers (for/list ([i (in-range (length (hash-keys table-header-hash)))])
                                    (hash-ref table-header-hash i)))
    (define table-header-size (+ 12 (* (length table-headers) (send TableEntry size))))

    (define data-hash (mhash))
    (for/fold ([current-offset table-header-size])
              ([(table-header i) (in-indexed table-headers)])
      (define tag (escape-tag (· table-header tag)))
      (define bstr (hash-ref! data-hash i
                              (λ ()
                                (define es (+EncodeStream))
                                (define tag-codec (hash-ref table-codecs tag (λ () (raise-argument-error 'directory:preEncode "valid table tag" tag))))
                                (send tag-codec encode es (hash-ref (· this-val tables) tag))
                                (send es dump))))
      (define 32-bit-bstr (pad-to-32bit bstr))

      (cond
        [(hash-ref offset-ks i #f) => (λ (k) (hash-remove! offset-ks i)
                                        (k (mhash
                                            'tag (unescape-tag (· table-header tag))
                                            'checkSum 0
                                            'offset current-offset
                                            'length (bytes-length bstr))))]
        [else
         (+ (bytes-length 32-bit-bstr) current-offset)]))
    
    (hash-set! this-val 'data (for/list ([i (in-range (length (hash-keys data-hash)))])
                                        (pad-to-32bit (hash-ref data-hash i))))

    (define numTables (length table-headers))
    (define searchRange (* (floor (log (/ numTables (log 2)))) 16))
    (define entrySelector (floor (/ searchRange (log 2))))
    (define rangeShift (- (* numTables 16) searchRange))
    
    (hash-set*! this-val
                'tag "true"
                'numTables numTables
                'tables table-headers
                'searchRange searchRange
                'entrySelector rangeShift
                'rangeShift rangeShift)))
      

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

#;(test-module
   (define ip (open-input-file charter-path))
   (define decoded-dir (deserialize (read (open-input-file charter-directory-path))))
   (check-equal? (directory-decode ip) decoded-dir)
   (define es (+EncodeStream))
   ;(send Directory encode es decoded-dir)
   )