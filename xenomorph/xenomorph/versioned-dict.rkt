#lang debug racket/base
(require "base.rkt" "dict.rkt"
         racket/dict
         racket/match
         racket/class
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define x:versioned-dict%
  (class x:dict%
    (super-new)
    (init-field [(@type type)] [(@versions versions)])

    (unless (for/or ([proc (list integer? procedure? xenomorphic-type? symbol?)])
                    (proc @type))
      (raise-argument-error 'x:versioned-dict "integer, procedure, symbol, or xenomorphic" @type))
    (unless (and (dict? @versions) (andmap (λ (v) (or (dict? v) (x:dict? v))) (dict-values @versions)))
      (raise-argument-error 'x:versioned-dict "dict of dicts or structish" @versions))

    (define (select-field-set val)
      (define version-key
        (or (dict-ref val x:version-key #f)
            (raise-argument-error 'x:versioned-dict-encode "value for version key" x:version-key)))
      (define field-object
        (or (dict-ref @versions version-key #f)
            (raise-argument-error 'x:versioned-dict-encode (format "valid field version: ~v" (dict-keys @versions)) version-key)))
      (if (x:dict? field-object) (get-field fields field-object) field-object))

    (define/override (x:decode port parent [length 0])
      (define res (setup-private-fields port parent length))
      (define which-version (match @type
                              [(? integer? int) int]
                              [(? symbol? key) #:when parent (dict-ref parent key)]
                              [(? procedure? proc) #:when parent (proc parent)]
                              [(or (? symbol?) (? procedure?))
                               (raise-argument-error 'x:versioned-dict-decode "valid parent" parent)]
                              [_ (send @type x:decode port parent)]))
      (dict-set! res x:version-key which-version)

      (match (dict-ref @versions 'header #f)
        [#false (void)]
        [header-val (parse-fields port res header-val)])
      
      (match (dict-ref @versions which-version #f)
        [#false (raise-argument-error 'x:versioned-dict-decode
                                      (format "valid field version: ~v" (dict-keys @versions)) which-version)]
        [(? x:versioned-dict? vs) (send vs x:decode port parent)]
        [field-object (parse-fields port res field-object)]))

    (define/override (pre-encode val) val)

    (define/override (x:encode field-data port [parent-arg #f])
      (unless (dict? field-data)
        (raise-argument-error 'x:versioned-dict-encode "dict" field-data))
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
        (raise-argument-error 'x:versioned-dict-encode (format "hash that contains superset of xversioned-dict keys: ~a" (dict-keys fields)) (dict-keys field-data)))
      (for ([(key type) (in-dict fields)])
           (send type x:encode (dict-ref field-data key) port parent))
      (let loop ([i 0])
        (when (< i (length (dict-ref parent x:pointers-key)))
          (define ptr (list-ref (dict-ref parent x:pointers-key) i))
          (match ptr
            [(x:ptr type val parent) i (send type x:encode val port parent)])
          (loop (add1 i)))))
    
    (define/override (x:size [val-arg #f] [parent-arg #f] [include-pointers #t])
      (unless val-arg
        (raise-argument-error 'x:versioned-dict-size "value" val-arg))
      (define val (pre-encode val-arg))
      (define parent (mhasheq x:parent-key parent-arg
                              x:val-key val
                              x:pointer-size-key 0))
      (define version-size
        (match @type
          [(or (? symbol?) (? procedure?)) 0]
          [_ (send @type x:size (dict-ref val x:version-key) parent)]))
      
      (define header-size
        (for/sum ([(key type) (in-dict (dict-ref @versions 'header null))])
                 (send type x:size (and val (dict-ref val key)) parent)))
      (define fields-size
        (for/sum ([(key type) (in-dict (select-field-set val))])
                 (send type x:size (and val (dict-ref val key)) parent)))
      (define pointer-size (if include-pointers (dict-ref parent x:pointer-size-key) 0))
      (+ version-size header-size fields-size pointer-size))))

(define (x:versioned-dict? x) (is-a? x x:versioned-dict%))

(define (x:versioned-dict type
                            [versions (dictify)]
                            #:pre-encode [pre-proc #f]
                            #:post-decode [post-proc #f]
                            #:base-class [base-class x:versioned-dict%])
  (new (generate-subclass base-class pre-proc post-proc)
       [type type]
       [versions versions]
       [fields #f]))


;; bw compat
(define x:versioned-struct% x:versioned-dict%)
(define x:versioned-struct? x:versioned-dict?)
(define x:versioned-struct x:versioned-dict)