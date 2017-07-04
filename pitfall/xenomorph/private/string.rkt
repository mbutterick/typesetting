#lang reader (submod "racket.rkt" reader)
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define (read-encoded-string port len [encoding 'ascii])
      (define proc (caseq encoding
                          [(utf16le) (error 'bah)]
                          [(ucs2) (error 'bleh)]
                          [(utf8) bytes->string/utf-8]
                          [(ascii) bytes->string/latin-1]
                          [else identity]))
      (proc (read-bytes len port)))

(define (write-encoded-string port string [encoding 'ascii])
      ;; todo: handle encodings correctly.
      ;; right now just utf8 and ascii are correct
      (caseq encoding
             [(utf16le ucs2 utf8 ascii) (write-bytes (string->bytes/utf-8 string) port)
                                        (when (eq? encoding 'utf16le)
                                          (error 'swap-bytes-unimplemented))]
             [else (error 'unsupported-string-encoding)]))

(define (count-nonzero-chars port)
      ;; helper function for String
      ;; counts nonzero chars from current position
      (length (car (regexp-match-peek "[^\u0]*" port))))

(define (byte-length val encoding)
  (define encoder
    (caseq encoding
           [(ascii utf8) string->bytes/utf-8]))
  (bytes-length (encoder (format "~a" val))))

(define (bytes-left-in-port? port)
  (not (eof-object? (peek-byte port))))

(define-subclass xenomorph-base% (StringT [len #f] [encoding 'ascii])
         
  (define/augment (decode port [parent #f])
    (let ([len (or (resolve-length len port parent) (count-nonzero-chars port))]
          [encoding (if (procedure? encoding)
                        (or (encoding parent) 'ascii)
                        encoding)]
          [adjustment (if (and (not len) (bytes-left-in-port? port)) 1 0)])
      (define string (read-encoded-string port len encoding))
      (pos port (+ (pos port) adjustment))
      string))
    

  (define/augment (encode port val [parent #f])
    (let* ([val (format "~a" val)]
           [encoding (if (procedure? encoding)
                         (or (encoding (and parent (· parent val)) 'ascii))
                         encoding)])
      (when (NumberT? len)
        (send len encode port (byte-length val encoding)))
      (write-encoded-string port val encoding)
      (when (not len) (write-byte #x00 port)))) ; null terminated when no len

  
  (define/augment (size [val #f] [parent #f])
    (if (not val)
        (resolve-length len #f parent)
        (let* ([encoding (if (procedure? encoding)
                             (or (encoding (and parent (· parent val)) 'ascii))
                             encoding)]
               [encoding (if (eq? encoding 'utf16be) 'utf16le encoding)])
          (+ (byte-length val encoding) (cond
                                          [(not len) 1]
                                          [(NumberT? len) (send len size)]
                                          [else 0]))))))


(define-values (String? +String) (values StringT? +StringT))

(test-module
   (require "stream.rkt")
   (define S (+String uint8 'utf8))
   (check-equal? (send S decode #"\2BCDEF") "BC")
   (check-equal? (send S encode #f "Mike") #"\4Mike")
   (check-equal? (send (+String) size "foobar") 7)) ; null terminated when no len