#lang racket/base
(require racket/class racket/match "base.rkt" "util.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define (decode-string len port [encoding 'ascii])
  (define decoder (case encoding
                    [(utf16le ucs2) (error 'unsupported-string-encoding)]
                    [(utf8) bytes->string/utf-8]
                    [(ascii) bytes->string/latin-1]
                    [else values]))
  (decoder (read-bytes len port)))

(define (encode-string string [encoding 'ascii])
  (define encoder (case encoding
                    [(ucs2 utf8 ascii) string->bytes/utf-8]
                    [(utf16le) (error 'swap-bytes-unimplemented)]
                    [else (error 'unsupported-string-encoding)]))
  (encoder string))

(define (count-nonzero-chars port)
  (bytes-length (car (regexp-match-peek "[^\u0]*" port))))

(define (bytes-left-in-port? port)
  (not (eof-object? (peek-byte port))))

(define x:string%
  (class xenobase%
    (super-new)
    (init-field [(@len len)] [(@encoding encoding)])

    (unless (length-resolvable? @len)
      (raise-argument-error 'xstring "length-resolvable?" @len))
    (unless (or (procedure? @encoding) (memq @encoding supported-encodings))
      (raise-argument-error 'xstring (format "procedure or member of ~v" supported-encodings) @encoding))

    (define/augment (decode port parent)
      (define len (or (resolve-length @len port parent) (count-nonzero-chars port)))
      (define encoding (match @encoding
                         [(? procedure? proc) (or (proc parent) 'ascii)]
                         [enc enc]))
      (define adjustment (if (and (not @len) (bytes-left-in-port? port)) 1 0))
      (begin0
        (decode-string len port encoding)
        (pos port (+ (pos port) adjustment))))

    (define/augment (encode val-arg port [parent #f])
      (define val (if (string? val-arg) val-arg (format "~a" val-arg)))
      (define encoding (match @encoding
                         [(? procedure?) (@encoding (and parent (hash-ref parent val)) 'ascii)]
                         [enc enc]))
      (define encoded-str (encode-string val encoding))
      (define encoded-length (bytes-length encoded-str))
      (when (and (exact-nonnegative-integer? @len) (> encoded-length @len))
        (raise-argument-error 'xstring-encode (format "string no longer than ~a" @len) val)) 
      (when (x:int? @len)
        (send @len encode encoded-length port parent))
      (define string-terminator (if @len (bytes) (bytes 0))) ; null terminated when no len
      (bytes-append encoded-str string-terminator)) 
    
    (define/augment (size [val-arg #f] [parent #f])
      (define val (cond
                    [(string? val-arg) val-arg]
                    [(not val-arg) #false]
                    [else (format "~a" val-arg)]))
      (cond
        [val (define encoding (if (procedure? @encoding)
                                  (or (@encoding (and parent (hash-ref parent val)) 'ascii))
                                  @encoding))
             (define string-size (bytes-length (encode-string val encoding)))
             (define strlen-size (cond
                                   [(not @len) 1]
                                   [(x:int? @len) (send @len size)]
                                   [else 0]))
             (+ string-size strlen-size)]
        [else (resolve-length @len #f parent)]))))

(define supported-encodings '(ascii utf8))
(define (x:string [len-arg #f] [enc-arg #f]
                  #:length [len-kwarg #f]
                  #:encoding [enc-kwarg #f]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:string%])
  (define len (or len-arg len-kwarg))
  (define encoding (or enc-arg enc-kwarg 'ascii))
  (new (generate-subclass base-class pre-proc post-proc) [len len] [encoding encoding]))

(define x:symbol%
  (class x:string%
    (super-new)
    (inherit-field len encoding)

    (define/override (pre-encode val)
      (unless (or (string? val) (symbol? val))
        (raise-argument-error 'xsymbol-encode "symbol or string" val))
      (if (symbol? val) (symbol->string val) val))
    
    (define/override (post-decode val) (string->symbol val))))

(define (x:symbol [len-arg #f] [enc-arg #f]
                  #:length [len-kwarg #f]
                  #:encoding [enc-kwarg #f]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:symbol%])
  (define len (or len-arg len-kwarg))
  (define encoding (or enc-arg enc-kwarg 'utf8))
  (new (generate-subclass base-class pre-proc post-proc) [len len] [encoding encoding]))

(module+ test
  (require rackunit "base.rkt")
  (define S-fixed (x:string 4 'utf8))
  (check-equal? (encode S-fixed "Mike" #f) #"Mike")
  (check-exn exn:fail? (λ () (encode S-fixed "Mikes" #f))) ; too long for fixed string 
  (define S (x:string uint8 'utf8))
  (check-equal? (decode S #"\2BCDEF") "BC")
  (check-equal? (encode S "Mike" #f) #"\4Mike")
  (check-equal? (size (x:string) "foobar") 7) ; null terminated when no len
  (check-equal? (decode (x:symbol 4) #"Mike") 'Mike)
  (check-equal? (encode (x:symbol 4) 'Mike #f) #"Mike")
  (check-equal? (encode (x:symbol 4) "Mike" #f) #"Mike")
  (check-exn exn:fail:contract? (λ () (encode (x:symbol 4) 42 #f))))