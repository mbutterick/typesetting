#lang restructure/racket
(require "number.rkt" (prefix-in utils- "utils.rkt") "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/String.coffee
|#

(define (byteLength val encoding)
  (define encoder
    (caseq encoding
         [(ascii utf8) string->bytes/utf-8]))
  (bytes-length (encoder val)))

(define-subclass Streamcoder (StringT [length_ #f] [encoding_ 'ascii])
         
  (define/augment (decode stream [parent #f])
    (define length__
      (cond
        [length_ (utils-resolveLength length_ stream parent)]
        [else (send stream count-nonzero-chars)]))
    (define encoding__
      (cond
        [(procedure? encoding_) (or (encoding_ parent) 'ascii)]
        [else encoding_]))
    (define string (send stream readString length__ encoding__))
    (when (and (not length_) (< (send stream pos) (send stream length)))
      (send stream pos (add1 (send stream pos))))
    string)
    

  #;(define/augment (encode stream val [parent #f])
      (define bytes (($codec-encoder codec) (format "~a" val)))
    
      (when (Number? length_) ;; length-prefixed string
        (send length_ encode stream (bytes-length bytes)))
    
      (send stream write bytes))

  (define/override (size [val #f] [parent #f])
    ;; Use the defined value if no value was given
    (cond
      [(not val) (utils-resolveLength length_ #f parent)]
      [else
       (define encoding__
         (cond
           [(procedure? encoding_) (or (encoding_ (and parent (· parent val)) 'ascii))]
           [else encoding_]))
       (when (eq? encoding__ 'utf16be)
         (set! encoding__ 'utf16le))
       (define size (byteLength val encoding__))
       (when (NumberT? length_)
         (increment! size (send length_ size)))
       (when (not length_)
         (increment! size))
       size]))
    

  )

(define-values (String? +String) (values StringT? +StringT))

#;(test-module
   (require "stream.rkt")
   (define stream (+DecodeStream #"\2BCDEF"))
   (define S (+String uint8 'utf8))
   (check-equal? (send S decode stream) "BC")
   (define os (+EncodeStream))
   (send S encode os "Mike")
   (check-equal? (send os dump) #"\4Mike")
   (check-equal? (send (+String) size "foobar") 6))