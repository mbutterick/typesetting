#lang at-exp br

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
    (define/public (swapBytes buff)
      (define bufflen (bytes-length buff))
      (when (odd? bufflen)
        (raise-argument-error 'swapBytes "even number of bytes" (bytes-length buff)))
      (define newbuff (make-bytes bufflen))
      (for ([bidx (in-range 0 bufflen 2)])
           (bytes-set! newbuff bidx (bytes-ref buff (add1 bidx)))
           (bytes-set! newbuff (add1 bidx) (bytes-ref buff bidx)))
      newbuff)))


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

  (check-equal? (send o swapBytes #"foobar") #"ofbora"))

