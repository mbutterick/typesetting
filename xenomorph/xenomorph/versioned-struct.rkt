#lang debug racket/base
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
      (raise-argument-error '+xversioned-struct "integer, procedure, symbol, or xenomorphic" @type))
    (unless (and (dict? @versions) (andmap (λ (v) (or (dict? v) (x:struct? v))) (dict-values @versions)))
      (raise-argument-error '+xversioned-struct "dict of dicts or structish" @versions))

    (define version-getter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent) (dict-ref parent @type))]))

    (define version-setter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent version) (dict-set! parent @type version))]))

    (define (extract-fields-dict val)
      (define version-key
        (or (dict-ref val x:version-key #f)
            (raise-argument-error 'xversioned-struct-encode "value for version key" x:version-key)))
      (define field-object
        (or (dict-ref @versions version-key #f)
            (raise-argument-error 'xversioned-struct-encode (format "valid field version: ~v" (dict-keys @versions)) version-key)))
      (if (x:struct? field-object) (get-field fields field-object) field-object))

    (define/override (x:decode port parent [length 0])
      (define res (xstruct-setup port parent length))
      (dict-set! res x:version-key
                 (cond
                   [(integer? @type) @type]
                   [(or (symbol? @type) (procedure? @type))
                    (unless parent
                      (raise-argument-error 'xversioned-struct-decode "valid parent" parent))
                    (version-getter parent)]
                   [else (send @type x:decode port)]))
      
      (when (dict-ref @versions 'header #f)
        (xstruct-parse-fields port res (dict-ref @versions 'header)))
    
      (define fields
        (or (dict-ref @versions (dict-ref res x:version-key #f) #f)
            (raise-argument-error 'xversioned-struct-decode "valid version key" (cons version @versions))))
    
      (cond
        [(x:versioned-struct? fields) (send fields x:decode port parent)]
        [else (xstruct-parse-fields port res fields)
              res]))

    (define/override (x:encode encode-me port [parent-arg #f])
      (unless (dict? encode-me)
        (raise-argument-error 'xversioned-struct-encode "dict" encode-me))
      (define parent (mhash 'pointers null
                            'startOffset (pos port)
                            'parent parent-arg
                            'val encode-me
                            'pointerSize 0))
      (dict-set! parent 'pointerOffset (+ (pos port) (x:size encode-me parent #f)))
      (unless (or (symbol? @type) (procedure? @type))
        (send @type x:encode (dict-ref encode-me x:version-key #f) port parent))
      (define maybe-header-dict (dict-ref @versions 'header #f))
      (when maybe-header-dict
        (for ([(key type) (in-dict maybe-header-dict)])
             (send type x:encode (dict-ref encode-me key) port parent)))

      (define fields (extract-fields-dict encode-me))
      (unless (andmap (λ (key) (member key (dict-keys encode-me))) (dict-keys fields))
        (raise-argument-error 'xversioned-struct-encode (format "hash that contains superset of xversioned-struct keys: ~a" (dict-keys fields)) (hash-keys encode-me)))
      (for ([(key type) (in-dict fields)])
           (send type x:encode (dict-ref encode-me key) port parent))
      (for ([ptr (in-list (dict-ref parent 'pointers))])
           (send (dict-ref ptr 'type) x:encode (dict-ref ptr 'val) port (dict-ref ptr 'parent))))
    
    (define/override (x:size [val #f] [parent-arg #f] [include-pointers #t])
      (unless val
        (raise-argument-error 'xversioned-struct-size "value" val))
      (define parent (mhash 'parent parent-arg 'val val 'pointerSize 0))
      (define version-size
        (let ([struct-type @type])
          (if (or (symbol? struct-type) (procedure? struct-type))
              0
              (send @type x:size (dict-ref val x:version-key) parent))))
      (define header-size
        (for/sum ([(key type) (in-dict (or (dict-ref @versions 'header #f) null))])
                 (send type x:size (and val (dict-ref val key)) parent)))
      (define fields-size
        (for/sum ([(key type) (in-dict (extract-fields-dict val))])
                 (send type x:size (and val (dict-ref val key)) parent)))
      (define pointer-size (if include-pointers (dict-ref parent 'pointerSize) 0))
      (+ version-size header-size fields-size pointer-size))))

(define (x:versioned-struct? x) (is-a? x x:versioned-struct%))

(define (x:versioned-struct type [versions (dictify)]
                            #:pre-encode [pre-proc #f]
                            #:post-decode [post-proc #f])
  (new (generate-subclass x:versioned-struct% pre-proc post-proc) [type type] [versions versions][fields #f]))

