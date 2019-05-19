#lang racket/base
(require racket/class
         racket/match
         racket/contract
         "base.rkt"
         "util.rkt"
         "number.rkt")
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

(define (string-ascii? string)
  (for/and ([c (in-string string)])
              (<= 0 (char->integer c) 127)))

(define (encode-string string [encoding 'ascii])
  (when (eq? encoding 'ascii)
    (unless (string-ascii? string)
      (raise-argument-error 'encode "ascii string" string)))
  (define encoder (case encoding
                    [(ucs2 utf8 ascii) string->bytes/utf-8]
                    [(utf16le) (error 'swap-bytes-unimplemented)]
                    [else (error 'unsupported-string-encoding)]))
  (encoder string))

(define (count-nonzero-chars port)
  (bytes-length (car (regexp-match-peek "[^\u0]*" port))))

(define (bytes-left-in-port? port)
  (not (eof-object? (peek-byte port))))

(define (supported-encoding? x)
  (and (symbol? x) (memq x supported-encodings)))

(define x:string%
  (class x:base%
    (super-new)
    (init-field [(@len len)] [(@encoding encoding)])

    (unless (length-resolvable? @len)
      (raise-argument-error 'x:string "length-resolvable?" @len))
    (unless (or (procedure? @encoding) (supported-encoding? @encoding))
      (raise-argument-error 'x:string (format "procedure or member of ~v" supported-encodings) @encoding))

    (define/augment (x:decode port parent)
      (define len (or (resolve-length @len port parent) (count-nonzero-chars port)))
      (define encoding (match @encoding
                         [(? procedure? proc) (or (proc parent) 'ascii)]
                         [enc enc]))
      (define adjustment (if (and (not @len) (bytes-left-in-port? port)) 1 0))
              (define result (decode-string len port encoding))
        (pos port (+ (pos port) adjustment))
      (when (eq? @encoding 'ascii)
        (unless (string-ascii? result)
          (raise-result-error 'decode "ascii string" result)))
      result)

    (define/augment (x:encode val-arg port [parent #f])
      (define val (if (string? val-arg) val-arg (format "~a" val-arg)))
      (define encoding (match @encoding
                         [(? procedure?) (@encoding (and parent (hash-ref parent val)) 'ascii)] ; when does this happen?
                         [enc enc]))
      (define encoded-str (encode-string val encoding))
      (define encoded-length (bytes-length encoded-str))
      (when (and (exact-nonnegative-integer? @len) (> encoded-length @len))
        (raise-argument-error 'encode (format "string no longer than ~a" @len) val)) 
      (when (x:int? @len)
        (send @len x:encode encoded-length port parent))
      (define string-terminator (if @len (bytes) (bytes 0))) ; null terminated when no len
      (bytes-append encoded-str string-terminator)) 
    
    (define/augment (x:size [val-arg #f] [parent #f])
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
                                   [(x:int? @len) (send @len x:size)]
                                   [else 0]))
             (+ string-size strlen-size)]
        [else (resolve-length @len #f parent)]))))

(define supported-encodings '(ascii utf8))

(define (x:string? x) (is-a? x x:string%))

(define/contract (x:string
                  [len-arg #f]
                  [enc-arg #f]
                  #:length [len-kwarg #f]
                  #:encoding [enc-kwarg 'utf8]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:string%])
  (()
   ((or/c length-resolvable? #false)
    (or/c procedure? supported-encoding? #false)
    #:length (or/c length-resolvable? #false)
    #:encoding (or/c procedure? supported-encoding? #false)
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:string%)))
   . ->* .
   x:string?)
  (define len (or len-arg len-kwarg))
  (unless (length-resolvable? len)
    (raise-argument-error 'x:string "resolvable length" len))
  (define encoding (or enc-arg enc-kwarg))
  (unless (or (supported-encoding? encoding) (procedure? encoding))
    (raise-argument-error 'x:string "valid encoding value" encoding))
  (new (generate-subclass base-class pre-proc post-proc)
       [len len]
       [encoding encoding]))
