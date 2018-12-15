#lang debug racket/base
(require "helper.rkt" "struct.rkt"
         racket/dict
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define (xversioned-struct-decode . args)
  (dict->mutable-hash (apply xversioned-struct-xdecode args)))

(define/post-decode (xversioned-struct-xdecode xvs [port-arg (current-input-port)] #:parent [parent-arg #f] [length 0])
  (define port (->input-port port-arg))
  (define parent (or (current-parent) parent-arg))
  (define res (xstruct-setup port parent length))

  (dict-set! res 'version
             (cond
               [(integer? (xversioned-struct-type xvs)) (xversioned-struct-type xvs)]
               #;[forced-version] ; for testing purposes: pass an explicit version
               [(or (symbol? (xversioned-struct-type xvs)) (procedure? (xversioned-struct-type xvs)))
                (unless parent
                  (raise-argument-error 'xversioned-struct-decode "valid parent" parent))
                ((xversioned-struct-version-getter xvs) parent)]
               [else (xdecode (xversioned-struct-type xvs) port)]))

  (when (dict-ref (xversioned-struct-versions xvs) 'header #f)
    (xstruct-parse-fields port res (dict-ref (xversioned-struct-versions xvs) 'header)))
    
  (define fields (or (dict-ref (xversioned-struct-versions xvs) (dict-ref res 'version #f) #f)
                     (raise-argument-error 'xversioned-struct-decode "valid version key" (cons version (xversioned-struct-versions xvs)))))
    
  (cond
    [(xversioned-struct? fields) (xdecode fields port #:parent parent)]
    [else (xstruct-parse-fields port res fields)
          res]))

(define (extract-fields-dict xvs val)
  (define field-object (dict-ref (xversioned-struct-versions xvs) (dict-ref val 'version #f) #f))
  (unless field-object
    (raise-argument-error 'xversioned-struct-encode "valid version key" version))
  (if (xstruct? field-object) (xstruct-fields field-object) field-object))

(define/finalize-size (xversioned-struct-size xvs [val #f] #:parent [parent-arg #f] [include-pointers #t])
  (unless val
    (raise-argument-error 'xversioned-struct-size "value" val))
  (define parent (mhash 'parent parent-arg 'val val 'pointerSize 0))
  (define version-size
    (let ([struct-type (xversioned-struct-type xvs)])
      (if (or (symbol? struct-type) (procedure? struct-type))
          0
          (size (xversioned-struct-type xvs) (dict-ref val 'version) #:parent parent))))
  (define header-size
    (for/sum ([(key type) (in-dict (or (dict-ref (xversioned-struct-versions xvs) 'header #f) null))])
             (size type (and val (dict-ref val key)) #:parent parent)))
  (define fields-size
    (for/sum ([(key type) (in-dict (extract-fields-dict xvs val))])
             (size type (and val (dict-ref val key)) #:parent parent)))
  (define pointer-size (if include-pointers (dict-ref parent 'pointerSize) 0))
  (+ version-size header-size fields-size pointer-size))

(define/pre-encode (xversioned-struct-encode xvs encode-me [port-arg (current-output-port)]
                                             #:parent [parent-arg #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    (unless (dict? encode-me)
      (raise-argument-error 'xversioned-struct-encode "dict" encode-me))
    (define parent (mhash 'pointers null
                          'startOffset (pos port)
                          'parent parent-arg
                          'val encode-me
                          'pointerSize 0))
    (dict-set! parent 'pointerOffset (+ (pos port) (xversioned-struct-size xvs encode-me #:parent parent #f)))
    (unless (or (symbol? (xversioned-struct-type xvs)) (procedure? (xversioned-struct-type xvs)))
      (encode (xversioned-struct-type xvs) (dict-ref encode-me 'version #f)))
    (define maybe-header-dict (dict-ref (xversioned-struct-versions xvs) 'header #f))
    (when maybe-header-dict
      (for ([(key type) (in-dict maybe-header-dict)])
           (encode type (dict-ref encode-me key) #:parent parent)))

    (define fields (extract-fields-dict xvs encode-me))
    (unless (andmap (λ (key) (member key (dict-keys encode-me))) (dict-keys fields))
      (raise-argument-error 'xversioned-struct-encode (format "hash that contains superset of xversioned-struct keys: ~a" (dict-keys fields)) (hash-keys encode-me)))
    (for ([(key type) (in-dict fields)])
         (encode type (dict-ref encode-me key) #:parent parent))
    (for ([ptr (in-list (dict-ref parent 'pointers))])
         (encode (dict-ref ptr 'type) (dict-ref ptr 'val) #:parent (dict-ref ptr 'parent)))
    (unless port-arg (get-output-bytes port))))

(struct xversioned-struct structish (type versions version-getter version-setter) #:transparent #:mutable
  #:methods gen:xenomorphic
  [(define decode xversioned-struct-decode)
   (define xdecode xversioned-struct-xdecode)
   (define encode xversioned-struct-encode)
   (define size xversioned-struct-size)])

(define (+xversioned-struct type [versions (dictify)])
  (unless (for/or ([proc (list integer? procedure? xenomorphic? symbol?)])
                  (proc type))
    (raise-argument-error '+xversioned-struct "integer, procedure, symbol, or xenomorphic" type))
  (unless (and (dict? versions) (andmap (λ (v) (or (dict? v) (structish? v))) (dict-values versions)))
    (raise-argument-error '+xversioned-struct "dict of dicts or structish" versions))
  (define version-getter (cond
                           [(procedure? type) type]
                           [(symbol? type) (λ (parent) (dict-ref parent type))]))
  (define version-setter (cond
                           [(procedure? type) type]
                           [(symbol? type) (λ (parent version) (dict-set! parent type version))]))
  (xversioned-struct type versions version-getter version-setter))

