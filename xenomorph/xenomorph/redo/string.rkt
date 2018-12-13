#lang debug racket/base
(require racket/dict "helper.rkt" "util.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define (read-encoded-string len [encoding 'ascii])
  (define proc (case encoding
                 [(utf16le) (error 'bah)]
                 [(ucs2) (error 'bleh)]
                 [(utf8) bytes->string/utf-8]
                 [(ascii) bytes->string/latin-1]
                 [else values]))
  (proc (read-bytes len)))

(define (write-encoded-string string [encoding 'ascii])
  ;; todo: handle encodings correctly.
  ;; right now just utf8 and ascii are correct
  (define proc (case encoding
                 [(ucs2 utf8 ascii) string->bytes/utf-8]
                 [(utf16le) (error 'swap-bytes-unimplemented)]
                 [else (error 'unsupported-string-encoding)]))
  (write-bytes (proc string)))

(define (count-nonzero-chars port)
  ;; helper function for String
  ;; counts nonzero chars from current position
  (bytes-length (car (regexp-match-peek "[^\u0]*" port))))

(define (bytes-left-in-port? port)
  (not (eof-object? (peek-byte port))))

(define (byte-length val encoding)
  (define encoder
    (case encoding
      [(ascii utf8) string->bytes/utf-8]))
  (bytes-length (encoder (format "~a" val))))

(define/post-decode (xstring-decode xs [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    (let ([len (or (resolve-length (xstring-len xs) #:parent parent) (count-nonzero-chars port))]
          [encoding (if (procedure? (xstring-encoding xs))
                        (or ((xstring-encoding xs) parent) 'ascii)
                        (xstring-encoding xs))]
          [adjustment (if (and (not (xstring-len xs)) (bytes-left-in-port? port)) 1 0)])
      (define string (read-encoded-string len encoding))
      (pos port (+ (pos port) adjustment))
      string)))

(define/pre-encode (xstring-encode xs val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    (let* ([val (format "~a" val)]
           [encoding (if (procedure? (xstring-encoding xs))
                         (or ((xstring-encoding xs) (and parent (dict-ref parent val)) 'ascii))
                         (xstring-encoding xs))])
      (define encoded-length (byte-length val encoding))
      (when (and (exact-nonnegative-integer? (xstring-len xs)) (> encoded-length (xstring-len xs)))
        (raise-argument-error 'xstring-encode (format "string no longer than ~a" (xstring-len xs)) val)) 
      (when (xint? (xstring-len xs))
        (encode (xstring-len xs) encoded-length))
      (write-encoded-string val encoding)
      (when (not (xstring-len xs)) (write-byte #x00)) ; null terminated when no len
      (unless port-arg (get-output-bytes port))))) 

(define/finalize-size (xstring-size xs [val #f] #:parent [parent #f])
  (cond
    [val (define encoding (if (procedure? (xstring-encoding xs))
                              (or ((xstring-encoding xs) (and parent (dict-ref parent val)) 'ascii))
                              (xstring-encoding xs)))
         (define string-size (byte-length val (if (eq? encoding 'utf16be) 'utf16le encoding)))
         (define strlen-size (cond
                               [(not (xstring-len xs)) 1]
                               [(xint? (xstring-len xs)) (size (xstring-len xs))]
                               [else 0]))
         (+ string-size strlen-size)]
    [else (resolve-length (xstring-len xs) #f #:parent parent)]))

(struct xstring xbase (len encoding) #:transparent
  #:methods gen:xenomorphic
  [(define decode xstring-decode)
   (define encode xstring-encode)
   (define size xstring-size)])

(define supported-encodings '(ascii utf8))
(define (+xstring [len-arg #f] [enc-arg #f]
                  #:length [len-kwarg #f] #:encoding [enc-kwarg #f])
  (define len (or len-arg len-kwarg))
  (define encoding (or enc-arg enc-kwarg 'ascii))
  (unless (length-resolvable? len)
    (raise-argument-error '+xarray "length-resolvable?" len))
  (unless (or (procedure? encoding) (memq encoding supported-encodings))
    (raise-argument-error '+xarray (format "procedure or member of ~v" supported-encodings) encoding))
  (xstring len encoding))

(define (xsymbol-decode xs [port-arg (current-input-port)] #:parent [parent #f])
  (string->symbol (xstring-decode xs port-arg #:parent parent)))

(define (xsymbol-encode xs val [port (current-output-port)] #:parent [parent #f])
  (unless (xsymbol? xs)
    (raise-argument-error 'encode "xsymbol instance" xs))
  (unless (or (string? val) (symbol? val))
    (raise-argument-error 'xsymbol-encode "symbol or string" val))
  (xstring-encode xs (if (symbol? val) val (string->symbol val)) port #:parent parent))

(struct xsymbol xstring () #:transparent
  #:methods gen:xenomorphic
  [(define decode xsymbol-decode)
   (define encode xsymbol-encode)
   (define size xstring-size)])

(define (+xsymbol [len-arg #f] [enc-arg #f]
                  #:length [len-kwarg #f] #:encoding [enc-kwarg #f])
  (define len (or len-arg len-kwarg))
  (define encoding (or enc-arg enc-kwarg 'ascii))
  (xsymbol len encoding))

(module+ test
  (require rackunit)
  (define S-fixed (+xstring 4 'utf8))
  (check-equal? (encode S-fixed "Mike" #f) #"Mike")
  (check-exn exn:fail? (λ () (encode S-fixed "Mikes" #f))) ; too long for fixed string 
  (define S (+xstring uint8 'utf8))
  (check-equal? (decode S #"\2BCDEF") "BC")
  (check-equal? (encode S "Mike" #f) #"\4Mike")
  (check-equal? (size (+xstring) "foobar") 7) ; null terminated when no len
  (check-equal? (decode (+xsymbol 4) #"Mike") 'Mike)
  (check-equal? (encode (+xsymbol 4) 'Mike #f) #"Mike")
  (check-equal? (encode (+xsymbol 4) "Mike" #f) #"Mike")
  (check-exn exn:fail:contract? (λ () (encode (+xsymbol 4) 42 #f))))