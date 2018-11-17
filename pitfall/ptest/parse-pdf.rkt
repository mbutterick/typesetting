#lang debug racket

(define pdf (file->bytes "test11rkt.pdf"))
(define pdf2 (file->bytes "test11rkt copy.pdf"))

(define (xref-offset pdf)
  (match (regexp-match #px"(?<=startxref\n)\\d+" pdf)
    [(list val) (read (open-input-bytes val))]
    [_ (error 'no-xref)]))

(define (parse-1-pdf-item x)
  (cond
    ;; obj name
    [(regexp-try-match #px"^\\d+ 0 obj" x) #false]
    ;; stream
    [(regexp-try-match #px"^stream\n(.*?)\nendstream" x) => cadr]
    ;; dict
    [(regexp-try-match #px"^<<\n(.*)\n>>" x)
     => (λ (m)
          (define items (parse-pdf-items (cadr m)))
          (unless (even? (length items))
            (raise items))
          (sort
           (for/list ([kv (in-slice 2 items)])
             (apply cons kv))
           bytes<?
           #:key car))]
    ;; list
    [(regexp-try-match #px"^\\[(.*?)\\]" x) => (compose1 parse-pdf-items cadr)]
    ;; xref
    [(regexp-try-match #px"^\\d+ 0 R" x) => car]
    ;; number
    [(regexp-try-match #px"^[-]?\\d+" x) => car]
    ;; parenstring
    [(regexp-try-match #px"^\\(.*?\\)" x) => car]
    ;; keystring
    [(regexp-try-match #px"^/\\S+" x) => car]
    ;; whitespace
    [(regexp-match #px"\\s" x) #false]
    [else eof]))

(define (parse-pdf-items bs)
  (for/list ([tok (in-port parse-1-pdf-item (open-input-bytes bs))]
             #:when tok)
    tok))

(define (pdf->dict pdf)
  (define xoff (xref-offset pdf))
  (define ip (open-input-bytes pdf))
  (void (read-bytes xoff ip))
  (define ref-count
    (match (regexp-match #px"(?<=xref\n0 )\\d+" ip)
      [(list val) (read (open-input-bytes val))]
      [_ (error 'no-xref-count)]))
  (define obj-locations
    (append
     ;; sort by byte offset
     (sort
      (cdr ; drop zeroth record: there is no zeroth object
       (for/list ([i ref-count])
         (cons i (read (open-input-bytes (car (regexp-match #px"\\d{10}" ip))))))) < #:key cdr)
     (list (cons #f xoff))))
  (define ip2 (open-input-bytes pdf))
  (sort
   (hash->list
    (for/hash ([(idx start) (in-dict obj-locations)]
               [(_ end) (in-dict (cdr obj-locations))])
      (values idx (parse-pdf-items (peek-bytes (- end start) start ip2)))))
   < #:key car))

(equal?
 (pdf->dict pdf)
 (pdf->dict pdf2)
 )