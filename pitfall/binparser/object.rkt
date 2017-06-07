#lang br

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise-argument-error 'read-bytes-exact (format "byte string length ~a" count) bs))
  bs)

(define BinaryIO%
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    (abstract size)))


(define ByteIO%
  (class BinaryIO%
    (super-new)
    (init-field [_count 1])
    (field [_bytes null])

    (define/override (decode ip)
      (set! _bytes (read-bytes-exact _count ip)))

    (define/override (encode op val) (write-bytes _bytes op))

    (define/override (size) (bytes-length _bytes))))

(define b (make-object ByteIO%))

(define ip (open-input-bytes #"ABC"))

(send b decode ip)

(define-macro (define-subclass SUPERCLASS (ID . INIT-ARGS) . BODY)
  #'(define ID (class SUPERCLASS (super-new) (init-field . INIT-ARGS) . BODY)))

(define-macro (getter-field [ID . EXPRS])
  (with-pattern ([_ID (prefix-id "_" #'ID)])
    #'(begin
        (field [(ID _ID)  . EXPRS])
        (public (_ID ID))
        (define (_ID) ID))))

(define (ends-with-8? type)
  (equal? (substring type (sub1 (string-length type))) "8"))

(define-subclass BinaryIO% (NumberT type [endian (if (system-big-endian?) 'BE 'LE)])
  (getter-field [fn (format "~a~a" type (if (ends-with-8? type)
                                            ""
                                            endian))])

  (define/override (decode ip) 'foo)

  (define/override (encode op val) 'foo)

  (define/override (size) 'foo))
    

(define o (make-object NumberT "UInt16"))

(send o fn)




#|
(define uint32be (:bytes 4 #:type integer/be?))
(define uint16be (:bytes 2 #:type integer/be?))
(define hexbytes (:bytes 4 #:type hex?))
(define (:make-string count) (:bytes count #:type string/ascii?))

(require (for-syntax sugar/debug))
(define-macro (:seq ([ID BINDING . MAYBE-GUARD] ...) . BODY)
  (with-pattern ([(GUARD ...) (pattern-case-filter #'(MAYBE-GUARD ...)
                                                   [(#:assert PRED) #'(λ (x) (unless (PRED x) (error 'assert-failed)))]
                                                   [ELSE #'void])])
                #'(λ (p) (let* ([ID (let ([ID (BINDING p)])
                                      (GUARD ID)
                                      ID)] ...)
                           (begin . BODY)
                           (list (cons 'ID ID) ...)))))

|#