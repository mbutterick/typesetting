#lang debug racket/base
(require "base.rkt" "struct.rkt"
         racket/dict
         racket/match
         racket/class
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define x:versioned-struct%
  (class x:struct%
    (super-new)
    (init-field [(@type type)] [(@versions versions)])

    (unless (for/or ([proc (list integer? procedure? xenomorphic-type? symbol?)])
              (proc @type))
      (raise-argument-error 'x:versioned-struct "integer, procedure, symbol, or xenomorphic" @type))
    (unless (and (dict? @versions) (andmap (λ (v) (or (dict? v) (x:struct? v))) (dict-values @versions)))
      (raise-argument-error 'x:versioned-struct "dict of dicts or structish" @versions))

    (define (select-field-set val)
      (define version-key
        (or (dict-ref val x:version-key #f)
            (raise-argument-error 'x:versioned-struct-encode "value for version key" x:version-key)))
      (define field-object
        (or (dict-ref @versions version-key #f)
            (raise-argument-error 'x:versioned-struct-encode (format "valid field version: ~v" (dict-keys @versions)) version-key)))
      (if (x:struct? field-object) (get-field fields field-object) field-object))

    (define/override (decode port parent [length 0])
      (define res (setup-private-fields port parent length))
      (define which-version (match @type
                              [(? integer? int) int]
                              [(? symbol? key) #:when parent (dict-ref parent key)]
                              [(? procedure? proc) #:when parent (proc parent)]
                              [(or (? symbol?) (? procedure?))
                               (raise-argument-error 'x:versioned-struct-decode "valid parent" parent)]
                              [_ (send @type decode port parent)]))
      (dict-set! res x:version-key which-version)

      (cond
        [(dict-ref @versions 'header #f)
         => (λ (header-val) (parse-fields port res header-val))])
      
      (define field-object
        (cond
          [(dict-ref @versions which-version #f) => values]
          [else
           (raise-argument-error 'x:versioned-struct-decode (format "valid field version: ~v" (dict-keys @versions)) which-version)]))
      
      (match field-object
        [(? x:versioned-struct?) (send field-object decode port parent)]
        [_ (parse-fields port res field-object)]))

    (define/override (pre-encode val) val)

    (define/override (encode field-data port [parent-arg #f])
      (unless (dict? field-data)
        (raise-argument-error 'x:versioned-struct-encode "dict" field-data))
      (define parent (mhasheq x:pointers-key null
                              x:start-offset-key (pos port)
                              x:parent-key parent-arg
                              x:val-key field-data
                              x:pointer-size-key 0))
      (hash-set! parent x:pointer-offset-key (+ (pos port) (size field-data parent #f)))
      (unless (or (symbol? @type) (procedure? @type))
        (send @type encode (dict-ref field-data x:version-key #f) port parent))
      (for ([(key type) (in-dict (dict-ref @versions 'header null))])
        (send type encode (dict-ref field-data key) port parent))

      (define fields (select-field-set field-data))
      (unless (andmap (λ (key) (member key (dict-keys field-data))) (dict-keys fields))
        (raise-argument-error 'x:versioned-struct-encode (format "hash that contains superset of xversioned-struct keys: ~a" (dict-keys fields)) (dict-keys field-data)))
      (for ([(key type) (in-dict fields)])
        (send type encode (dict-ref field-data key) port parent))
      (let loop ([i 0])
        (when (< i (length (dict-ref parent x:pointers-key)))
          (define ptr (list-ref (dict-ref parent x:pointers-key) i))
          (match ptr
            [(x:ptr type val parent) i (send type encode val port parent)])
          (loop (add1 i)))))
    
    (define/override (size [val-arg #f] [parent-arg #f] [include-pointers #t])
      (unless val-arg
        (raise-argument-error 'x:versioned-struct-size "value" val-arg))
      (define val (pre-encode val-arg))
      (define parent (mhasheq x:parent-key parent-arg
                              x:val-key val
                              x:pointer-size-key 0))
      (define version-size
        (match @type
          [(or (? symbol?) (? procedure?)) 0]
          [_ (send @type size (dict-ref val x:version-key) parent)]))
      
      (define header-size
        (for/sum ([(key type) (in-dict (dict-ref @versions 'header null))])
          (send type size (and val (dict-ref val key)) parent)))
      (define fields-size
        (for/sum ([(key type) (in-dict (select-field-set val))])
          (send type size (and val (dict-ref val key)) parent)))
      (define pointer-size (if include-pointers (dict-ref parent x:pointer-size-key) 0))
      (+ version-size header-size fields-size pointer-size))))

(define (x:versioned-struct? x) (is-a? x x:versioned-struct%))

(define (x:versioned-struct type [versions (dictify)]
                            #:pre-encode [pre-proc #f]
                            #:post-decode [post-proc #f]
                            #:base-class [base-class x:versioned-struct%])
  (new (generate-subclass base-class pre-proc post-proc) [type type] [versions versions][fields #f]))

