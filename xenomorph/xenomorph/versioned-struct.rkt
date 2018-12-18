#lang racket/base
(require "helper.rkt" "struct.rkt"
         racket/dict
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

    (define version-getter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent) (dict-ref parent @type))]))

    (define version-setter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent version) (dict-set! parent @type version))]))

    (define (select-field-set val)
      (define version-key
        (or (dict-ref val x:version-key #f)
            (raise-argument-error 'x:versioned-struct-encode "value for version key" x:version-key)))
      (define field-object
        (or (dict-ref @versions version-key #f)
            (raise-argument-error 'x:versioned-struct-encode (format "valid field version: ~v" (dict-keys @versions)) version-key)))
      (if (x:struct? field-object) (get-field fields field-object) field-object))

    (define/override (x:decode port parent [length 0])
      (define res (setup-private-fields port parent length))
      (define which-version (cond
                              [(integer? @type) @type]
                              [(or (symbol? @type) (procedure? @type))
                               (unless parent
                                 (raise-argument-error 'x:versioned-struct-decode "valid parent" parent))
                               (version-getter parent)]
                              [else (send @type x:decode port parent)]))
      (dict-set! res x:version-key which-version)

      (define maybe-header-val (dict-ref @versions 'header #f))
      (when maybe-header-val
        (parse-fields port res maybe-header-val))
      
      (define field-object
        (or (dict-ref @versions which-version #f)
            (raise-argument-error 'x:versioned-struct-decode (format "valid field version: ~v" (dict-keys @versions)) which-version)))
    
      (if (x:versioned-struct? field-object)
          (send field-object x:decode port parent)
          (parse-fields port res field-object)))

    (define/override (x:encode field-data port [parent-arg #f])
      (unless (dict? field-data)
        (raise-argument-error 'x:versioned-struct-encode "dict" field-data))
      (define parent (mhasheq x:pointers-key null
                              x:start-offset-key (pos port)
                              x:parent-key parent-arg
                              x:val-key field-data
                              x:pointer-size-key 0))
      (hash-set! parent x:pointer-offset-key (+ (pos port) (x:size field-data parent #f)))
      (unless (or (symbol? @type) (procedure? @type))
        (send @type x:encode (dict-ref field-data x:version-key #f) port parent))
      (for ([(key type) (in-dict (dict-ref @versions 'header null))])
        (send type x:encode (dict-ref field-data key) port parent))

      (define fields (select-field-set field-data))
      (unless (andmap (λ (key) (member key (dict-keys field-data))) (dict-keys fields))
        (raise-argument-error 'x:versioned-struct-encode (format "hash that contains superset of xversioned-struct keys: ~a" (dict-keys fields)) (hash-keys field-data)))
      (for ([(key type) (in-dict fields)])
        (send type x:encode (dict-ref field-data key) port parent))
      (for ([ptr (in-list (dict-ref parent x:pointers-key))])
        (send (x:ptr-type ptr) x:encode (x:ptr-val ptr) port (x:ptr-parent ptr))))
    
    (define/override (x:size [val #f] [parent-arg #f] [include-pointers #t])
      (unless val
        (raise-argument-error 'x:versioned-struct-size "value" val))
      (define parent (mhasheq x:parent-key parent-arg
                              x:val-key val
                              x:pointer-size-key 0))
      (define version-size
        (let ([struct-type @type])
          (if (or (symbol? struct-type) (procedure? struct-type))
              0
              (send @type x:size (dict-ref val x:version-key) parent))))
      (define header-size
        (for/sum ([(key type) (in-dict (or (dict-ref @versions 'header #f) null))])
          (send type x:size (and val (dict-ref val key)) parent)))
      (define fields-size
        (for/sum ([(key type) (in-dict (select-field-set val))])
          (send type x:size (and val (dict-ref val key)) parent)))
      (define pointer-size (if include-pointers (dict-ref parent x:pointer-size-key) 0))
      (+ version-size header-size fields-size pointer-size))))

(define (x:versioned-struct? x) (is-a? x x:versioned-struct%))

(define (x:versioned-struct type [versions (dictify)]
                            #:pre-encode [pre-proc #f]
                            #:post-decode [post-proc #f])
  (new (generate-subclass x:versioned-struct% pre-proc post-proc) [type type] [versions versions][fields #f]))

