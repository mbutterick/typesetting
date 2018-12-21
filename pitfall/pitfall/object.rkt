#lang racket/base
(require
  "struct.rkt"
  racket/class
  racket/string
  racket/contract
  racket/list
  sugar/unstable/js
  (only-in srfi/19 date->string))
(provide PDFObject convert)

(define PDFObject
  (class object%
    (super-new)))

;; moved `number` to helper module


(define escaped-chars '(#\newline #\return #\tab #\backspace #\page #\( #\) #\\))
(define escaped-char-strings '("\\n" "\\r" "\\t" "\\b" "\\f" "\\(" "\\)" "\\\\"))

;; note: unlike nodejs, escapableRe does not have `g` option built in
;; so use it with regexp-replace* not regexp-replace
(define escapableRe (regexp (format "[~a]" (regexp-quote (list->string escaped-chars)))))
(define escapable (for/hash ([k (in-list escaped-chars)]
                             [v (in-list escaped-char-strings)])
                    (values (string k) v)))


(define/contract (string-slice str length)
  (string? integer? . -> . string?)
  (substring str (if (negative? length)
                     (+ (string-length str) length)
                     length)))


(define/contract (pad str length)
  (string? integer? . -> . string?)
  (define newstr (string-append (string-join (make-list (add1 length) "") "0") str))
  (string-slice newstr (- length)))


;; Convert little endian UTF-16 to big endian
;; endianness of `bytes-open-converter` is relative to platform, so little endian on all x86
;; can detect with `system-big-endian?`
(define/contract (utf8->utf16 bytes)
  (bytes? . -> . bytes?)
  (define-values (bs bslen bsresult)
    (bytes-convert (bytes-open-converter "platform-UTF-8" "platform-UTF-16") bytes))
  bs)

(define/contract (swapBytes buff)
  (bytes? . -> . bytes?)
  (define bufflen (bytes-length buff))
  (when (odd? bufflen)
    (raise-argument-error 'swapBytes "even number of bytes" bufflen))
  (for/fold ([newbuff (make-bytes bufflen)])
            ([bidx (in-range bufflen)] #:when (even? bidx))
    (bytes-set! newbuff bidx (bytes-ref buff (add1 bidx)))
    (bytes-set! newbuff (add1 bidx) (bytes-ref buff bidx))
    newbuff))

(define/contract (convert object)
  (any/c . -> . string?)
  (let loop ([x object])
    (cond
      ;; String literals are converted to the PDF name type
      [(string? x) (string-append "/" x)]
      ;; symbols are used for convenience - convert to string
      [(symbol? x) (loop (symbol->string x))]
      ;; String objects (structs) are converted to PDF strings (UTF-16)
      [(String? x)
       ;; Escape characters as required by the spec
       (define string (regexp-replace* escapableRe (String-string x) (λ (c) (hash-ref escapable c))))
       ;; Detect if this is a unicode string (= contains non-ascii chars)
       (define contains-non-ascii? (for/or ([c (in-string string)])
                                     (char>? c (integer->char 127))))
       ;; If so, encode it as big endian UTF-16
       (format "(~a)" (if contains-non-ascii?
                          (bytes->string/latin-1 (swapBytes  (utf8->utf16 (string->bytes/utf-8 (string-append "\ufeff" string)))))
                          string))]
      ;; Buffers (= byte strings) are converted to PDF hex strings
      [(bytes? x) (format "<~a>" (string-append*
                                  (for/list ([b (in-bytes x)])
                                    (number->string b 16))))]
      [(object? x) (send x to-string)]
      [(date? x) (format "(D:~aZ)" (date->string x "~Y~m~d~H~M~S"))]
      [(list? x) (format "[~a]" (string-join (map loop x) " "))]
      [(hash? x) (string-join (append (list "<<")
                                      (for/list ([(k v) (in-hash x)])
                                        (format "~a ~a" (loop k) (loop v)))
                                      (list ">>"))
                              (string #\newline))]
      [(number? x) (format "~a" (number x))]
      [else (format "~a" x)])))
    

(module+ test
  (require rackunit )
  (define o (new PDFObject))
  (check-equal? (pad "foobar" -1) "oobar")
  (check-equal? (pad "foobar" 0) "foobar")
  (check-equal? (pad "foobar" 3) "bar")
  (check-equal? (pad "foobar" 6) "foobar")
  (check-equal? (pad "foobar" 10) "0000foobar")

  (check-equal? (regexp-replace* escapableRe "foo\nba\nr" "x") "fooxbaxr")
  (check-equal? (regexp-replace* escapableRe "foo\fba\tr" "x") "fooxbaxr")
  
  (check-equal? (regexp-replace* escapableRe "foo\nba\tr" (λ (c) (hash-ref escapable c))) "foo\\nba\\tr")

  (check-equal? (swapBytes #"foobar") #"ofbora")

  (check-equal? (convert "foobar") "/foobar")
  (check-equal? (convert (String "foobar")) "(foobar)")
  (check-equal? (convert (String "öéÿ")) "(þÿ\u0000ö\u0000é\u0000ÿ)")
  (check-equal? (convert (String "fôobár")) "(þÿ\u0000f\u0000ô\u0000o\u0000b\u0000á\u0000r)")
  (check-equal? (convert #"foobar") "<666f6f626172>")
  #;(check-equal? (convert (make-object PDFReference "foobar" 42)) "42 0 R")
  (check-equal? (convert (seconds->date (quotient 1494483337320 1000) #f)) "(D:20170511061537Z)")
  (check-equal? (convert (list "foobar" (String "öéÿ") #"foobar")) "[/foobar (þÿ\u0000ö\u0000é\u0000ÿ) <666f6f626172>]")
  #;(check-equal? (convert (hash "foo" 42 "bar" "fly")) "<<\n/foo 42\n/bar /fly\n>>")
  (check-equal? (convert 1234.56789) "1234.56789"))



