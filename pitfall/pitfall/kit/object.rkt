#lang at-exp br
(require "struct.rkt")

(define PDFObject
  (class object%
    (super-new)

    (define (string-slice str length)
      (if (negative? length)
          (string-slice str (+ (string-length str) length))
          (substring str length)))

    (define/public (pad str length)
      (define newstr (string-append (string-join (make-list (add1 length) "") "0") str))
      (string-slice newstr (- length)))

    (define escaped-chars '(#\newline #\return #\tab #\backspace #\page #\( #\) #\\))
    (define escaped-char-strings '("\\n" "\\r" "\\t" "\\b" "\\f" "\\(" "\\)" "\\\\"))

    ;; note: unlike nodejs, escapableRe does not have `g` option built in
    ;; so use it with regexp-replace* not regexp-replace
    (field [escapableRe
            (regexp (format "[~a]" (regexp-quote (list->string escaped-chars))))])

    (field [escapable (for/hash ([k (in-list escaped-chars)]
                                 [v (in-list escaped-char-strings)])
                        (values (string k) v))])

    ; Convert little endian UTF-16 to big endian
    (define (utf8->utf16 bytes)
      (let-values ([(bs bslen bsresult)
                    (bytes-convert (bytes-open-converter "platform-UTF-8" "platform-UTF-16") bytes)])
        bs))
    
    (define/public (swapBytes buff)
      (define bufflen (bytes-length buff))
      (when (odd? bufflen)
        (raise-argument-error 'swapBytes "even number of bytes" (bytes-length buff)))
      (define newbuff (make-bytes bufflen))
      (for ([bidx (in-range 0 bufflen 2)])
        (bytes-set! newbuff bidx (bytes-ref buff (add1 bidx)))
        (bytes-set! newbuff (add1 bidx) (bytes-ref buff bidx)))
      newbuff)

    (define/public (convert object)
      (cond
        ;; String literals are converted to the PDF name type
        [(string? object) (string-append "/" object)]
        ;; String objects are converted to PDF strings (UTF-16)
        [(String? object)
         ;; Escape characters as required by the spec
         (define string
           (regexp-replace* escapableRe (String-string object)
                            (λ (c) (hash-ref escapable c))))
         ;; Detect if this is a unicode string
         (define isUnicode
           (for/or ([c (in-string string)])
             (char>? c (integer->char #x7f))))
         ;; If so, encode it as big endian UTF-16
         (string-append "(" (if isUnicode
                                (bytes->string/latin-1 (swapBytes (utf8->utf16 (string->bytes/utf-8 (string-append "\ufeff" string)))))
                                string) ")")]
        ;; Buffers are converted to PDF hex strings
        [(bytes? object) (string-append "<" (string-append*
                                             (for/list ([b (in-bytes object)])
                                               (number->string b 16))) ">")]
        [else 42]))))


(module+ test
  (require rackunit)
  (define o (new PDFObject))
  (check-equal? (send o pad "foobar" -1) "oobar")
  (check-equal? (send o pad "foobar" 0) "foobar")
  (check-equal? (send o pad "foobar" 3) "bar")
  (check-equal? (send o pad "foobar" 6) "foobar")
  (check-equal? (send o pad "foobar" 10) "0000foobar")

  (check-equal? (regexp-replace* (get-field escapableRe o) "foo\nba\nr" "x") "fooxbaxr")
  (check-equal? (regexp-replace* (get-field escapableRe o) "foo\fba\tr" "x") "fooxbaxr")
  
  (check-equal? (regexp-replace* (get-field escapableRe o) "foo\nba\tr" (λ (c) (hash-ref (get-field escapable o) c))) "foo\\nba\\tr")

  (check-equal? (send o swapBytes #"foobar") #"ofbora")

  (check-equal? (send o convert "foobar") "/foobar")
  (check-equal? (send o convert (String "foobar")) "(foobar)")
  (check-equal? (send o convert (String "öéÿ")) "(þÿ\u0000ö\u0000é\u0000ÿ)")
  (check-equal? (send o convert (String "fôobár")) "(þÿ\u0000f\u0000ô\u0000o\u0000b\u0000á\u0000r)")
  (check-equal? (send o convert #"foobar") "<666f6f626172>"))



