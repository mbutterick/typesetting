#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define (byte-length val encoding)
  (define encoder
    (caseq encoding
           [(ascii utf8) string->bytes/utf-8]))
  (bytes-length (encoder (format "~a" val))))

(define-subclass Streamcoder (StringT [len #f] [encoding 'ascii])
         
  (define/augment (decode stream [parent #f])
    (let ([len (or (resolve-length len stream parent) (send stream count-nonzero-chars))]
          [encoding (if (procedure? encoding)
                        (or (encoding parent) 'ascii)
                        encoding)]
          [adjustment (if (and (not len) (< (· stream pos) (· stream length))) 1 0)])
      (define string (send stream readString len encoding))
      (send stream pos (+ (· stream pos) adjustment))
      string))
    

  (define/augment (encode stream val [parent #f])
    (let* ([val (format "~a" val)]
           [encoding (if (procedure? encoding)
                         (or (encoding (and parent (· parent val)) 'ascii))
                         encoding)])
      (when (NumberT? len)
        (send len encode stream (byte-length val encoding)))
      (send stream writeString val encoding)
      (when (not len) (send stream writeUInt8 #x00)))) ; null terminated when no len

  
  (define/override (size [val #f] [parent #f])
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
   (define stream (+DecodeStream #"\2BCDEF"))
   (define S (+String uint8 'utf8))
   (check-equal? (send S decode stream) "BC")
   (check-equal? (send S encode #f "Mike") #"\4Mike")
   (check-equal? (send (+String) size "foobar") 7)) ; null terminated when no len