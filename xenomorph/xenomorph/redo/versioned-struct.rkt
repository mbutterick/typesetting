#lang racket/base
(require "helper.rkt" "struct.rkt"
         racket/dict
         sugar/unstable/dict)
(provide (all-defined-out) decode/hash)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define/post-decode (xversioned-struct-decode xvs [port-arg (current-input-port)] #:parent [parent #f] [length 0])
  (define port (->input-port port-arg))
  (define res (_setup port parent length))

  (dict-set! res 'version
             (cond
               [(integer? (xversioned-struct-type xvs)) (xversioned-struct-type xvs)]
               #;[forced-version] ; for testing purposes: pass an explicit version
               [(or (symbol? (xversioned-struct-type xvs)) (procedure? (xversioned-struct-type xvs)))
                (unless parent
                  (raise-argument-error 'xversioned-struct-decode "valid parent" parent))
                ((xversioned-struct-version-getter xvs) parent)]
               [else (decode (xversioned-struct-type xvs) port)]))

  (when (dict-ref (xversioned-struct-versions xvs) 'header #f)
    (_parse-fields port res (dict-ref (xversioned-struct-versions xvs) 'header)))
    
  (define fields (or (dict-ref (xversioned-struct-versions xvs) (dict-ref res 'version #f) #f)
                     (raise-argument-error 'xversioned-struct-decode "valid version key" (cons version (xversioned-struct-versions xvs)))))
    
  (cond
     [(xversioned-struct? fields) (decode fields port #:parent parent)]
     [else (_parse-fields port res fields)
           res]))

(define/finalize-size (xversioned-struct-size xvs [val #f] #:parent [parent-arg #f] [include-pointers #t])
  (unless val
    (raise-argument-error 'xversioned-struct-size "value" val))
  (define parent (mhash 'parent parent-arg 'val val 'pointerSize 0))
  (define version-size
    (if (not (or (symbol? (xversioned-struct-type xvs)) (procedure? (xversioned-struct-type xvs))))
        (size (xversioned-struct-type xvs) (dict-ref val 'version) #:parent parent)
        0))
  (define header-size
    (for/sum ([(key type) (in-dict (or (dict-ref (xversioned-struct-versions xvs) 'header #f) null))])
      (size type (and val (dict-ref val key)) #:parent parent)))
  (define fields-size
    (let ([fields (or (dict-ref (xversioned-struct-versions xvs) (dict-ref val 'version))
                      (raise-argument-error 'xversioned-struct-size "valid version key" version))])
      (for/sum ([(key type) (in-dict fields)])
        (size type (and val (dict-ref val key)) #:parent parent))))
  (define pointer-size (if include-pointers (dict-ref parent 'pointerSize) 0))
  (+ version-size header-size fields-size pointer-size))

(define/pre-encode (xversioned-struct-encode xvs val [port-arg (current-output-port)] #:parent [parent-arg #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
  (unless (dict? val)
    (raise-argument-error 'xversioned-struct-encode "dict" val))

  (define parent (mhash 'pointers null
                     'startOffset (pos port)
                     'parent parent-arg
                     'val val
                     'pointerSize 0))
  (dict-set! parent 'pointerOffset (+ (pos port) (xversioned-struct-size xvs val #:parent parent #f)))

  (when (not (or (symbol? (xversioned-struct-type xvs)) (procedure? (xversioned-struct-type xvs))))
    (encode (xversioned-struct-type xvs) (dict-ref val 'version #f)))

  (when (dict-ref (xversioned-struct-versions xvs) 'header #f)
    (for ([(key type) (in-dict (dict-ref (xversioned-struct-versions xvs) 'header))])
      (encode type (dict-ref val key) #:parent parent)))

  (define fields (or (dict-ref (xversioned-struct-versions xvs) (dict-ref val 'version #f))
                     (raise-argument-error 'xversioned-struct-encode "valid version key" version)))

  (unless (andmap (λ (key) (member key (dict-keys val))) (dict-keys fields))
    (raise-argument-error 'xversioned-struct-encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys val)))

  (for ([(key type) (in-dict fields)])
    (encode type (dict-ref val key) #:parent parent))
  (for ([ptr (in-list (dict-ref parent 'pointers))])
    (encode (dict-ref ptr 'type) (dict-ref ptr 'val) #:parent (dict-ref ptr 'parent)))
  
  (unless port-arg (get-output-bytes port))))

(struct xversioned-struct structish (type versions version-getter version-setter) #:transparent #:mutable
  #:methods gen:xenomorphic
  [(define decode xversioned-struct-decode)
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

