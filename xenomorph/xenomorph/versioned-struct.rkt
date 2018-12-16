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

(define xversioned-struct%
  (class xstruct%
    (super-new)
    (init-field [(@type type)] [(@versions versions)])
    (inherit-field [@fields fields])

    (define version-getter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent) (dict-ref parent @type))]))

    (define version-setter (cond
                             [(procedure? @type) @type]
                             [(symbol? @type) (λ (parent version) (dict-set! parent @type version))]))

    (define (extract-fields-dict val)
      (define field-object (dict-ref @versions (dict-ref val 'version #f) #f))
      (unless field-object
        (raise-argument-error 'xversioned-struct-encode "valid version key" version))
      (if (xstruct? field-object) (get-field fields field-object) field-object))

    (define/augment (xxdecode port parent [length 0])
      (define res (xstruct-setup port parent length))
      (dict-set! res 'version
                 (cond
                   [(integer? @type) @type]
                   #;[forced-version] ; for testing purposes: pass an explicit version
                   [(or (symbol? @type) (procedure? @type))
                    (unless parent
                      (raise-argument-error 'xversioned-struct-decode "valid parent" parent))
                    (version-getter parent)]
                   [else (send @type xxdecode port)]))

      (when (dict-ref @versions 'header #f)
        (xstruct-parse-fields port res (dict-ref @versions 'header)))
    
      (define fields
        (or (dict-ref @versions (dict-ref res 'version #f) #f)
            (raise-argument-error 'xversioned-struct-decode "valid version key" (cons version @versions))))
    
      (cond
        [(xversioned-struct? fields) (send fields xxdecode port parent)]
        [else (xstruct-parse-fields port res fields)
              res]))

    (define/augment (xxencode encode-me port [parent-arg #f])
      (unless (dict? encode-me)
        (raise-argument-error 'xversioned-struct-encode "dict" encode-me))
      (define parent (mhash 'pointers null
                            'startOffset (pos port)
                            'parent parent-arg
                            'val encode-me
                            'pointerSize 0))
      (dict-set! parent 'pointerOffset (+ (pos port) (xxsize encode-me parent #f)))
      (unless (or (symbol? @type) (procedure? @type))
        (send @type xxencode (dict-ref encode-me 'version #f) port parent))
      (define maybe-header-dict (dict-ref @versions 'header #f))
      (when maybe-header-dict
        (for ([(key type) (in-dict maybe-header-dict)])
          (send type xxencode (dict-ref encode-me key) port parent)))

      (define fields (extract-fields-dict encode-me))
      (unless (andmap (λ (key) (member key (dict-keys encode-me))) (dict-keys fields))
        (raise-argument-error 'xversioned-struct-encode (format "hash that contains superset of xversioned-struct keys: ~a" (dict-keys fields)) (hash-keys encode-me)))
      (for ([(key type) (in-dict fields)])
        (send type xxencode (dict-ref encode-me key) port parent))
      (for ([ptr (in-list (dict-ref parent 'pointers))])
        (send (dict-ref ptr 'type) xxencode (dict-ref ptr 'val) port (dict-ref ptr 'parent))))
    
    (define/augment (xxsize [val #f] [parent-arg #f] [include-pointers #t])
      (unless val
        (raise-argument-error 'xversioned-struct-size "value" val))
      (define parent (mhash 'parent parent-arg 'val val 'pointerSize 0))
      (define version-size
        (let ([struct-type @type])
          (if (or (symbol? struct-type) (procedure? struct-type))
              0
              (send @type xxsize (dict-ref val 'version) parent))))
      (define header-size
        (for/sum ([(key type) (in-dict (or (dict-ref @versions 'header #f) null))])
          (send type xxsize (and val (dict-ref val key)) parent)))
      (define fields-size
        (for/sum ([(key type) (in-dict (extract-fields-dict val))])
          (send type xxsize (and val (dict-ref val key)) parent)))
      (define pointer-size (if include-pointers (dict-ref parent 'pointerSize) 0))
      (+ version-size header-size fields-size pointer-size))))

(define (xversioned-struct? x) (is-a? x xversioned-struct%))

(define (+xversioned-struct #:subclass [class xversioned-struct%] type [versions (dictify)])
  (unless (for/or ([proc (list integer? procedure? xenomorphic-type? symbol?)])
            (proc type))
    (raise-argument-error '+xversioned-struct "integer, procedure, symbol, or xenomorphic" type))
  (unless (and (dict? versions) (andmap (λ (v) (or (dict? v) (xstruct? v))) (dict-values versions)))
    (raise-argument-error '+xversioned-struct "dict of dicts or structish" versions))
  (new class [type type] [versions versions]))

