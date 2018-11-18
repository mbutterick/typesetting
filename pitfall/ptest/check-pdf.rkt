#lang debug racket
(require rackunit)
(provide (all-defined-out))

(define (xref-offset bs)
  (match (regexp-match #px"(?<=startxref\n)\\d+" bs)
    [(list val) (read (open-input-bytes val))]
    [_ (error 'no-xref)]))

(define-syntax (pat-lex stx)
  (syntax-case stx (else)
    [(_ PORT [PAT . REST] ... [else ELSE-CLAUSE])
     (with-syntax ([(REST ...)
                    (map (λ (stx) (syntax-case stx ()
                                    [() #'(=> car)]
                                    [_ stx])) (syntax->list #'(REST ...)))])
       #'(cond
           [(regexp-try-match (pregexp (string-append "^" PAT)) PORT) . REST] ... [else ELSE-CLAUSE]))]))

(define (parse-1 x)
  (pat-lex x
           ["\\s+" (parse-1 x)] ; whitespace
           ["\\d+ 0 obj" (parse-1 x)] ;; obj name
           ["(<<\n.*\n>>)\\s*stream\n" ; dict with stream
            => (λ (m)
                 (define d (parse-1 (open-input-bytes (cadr m))))
                 (define stream-length
                   (read (open-input-bytes (cdr (assoc #"/Length" d)))))
                 (define stream (read-bytes stream-length x))
                 (append d (list (cons 'stream stream))))]
           ["<<\n(.*)\n>>" ;; dict
            => (λ (m)
                 (define items (parse-pdf-bytes (cadr m)))
                 (unless (even? (length items))
                   (raise items))
                 (sort ; put hash into order so it's comparable
                  (for/list ([kv (in-slice 2 items)])
                    (apply cons kv))
                  bytes<?
                  #:key car))]
           ["\\[(.*?)\\]" => (compose1 parse-pdf-bytes cadr)] ;; list
           ["\\d+ 0 R"] ; xref
           ["[-]?\\d+"] ; number
           ["\\(.*?\\)"] ; parenstring
           ["/\\S+"] ; keystring
           [else eof]))

(define (parse-pdf-bytes bs)
  (for/list ([tok (in-port parse-1 (open-input-bytes bs))])
    tok))

(define (pdf->dict pdf)
  (define pdf-bs (file->bytes pdf))
  (define xoff (xref-offset pdf-bs))
  (define xref-ip (open-input-bytes (subbytes pdf-bs (+ xoff (bytes-length #"xref\n0")))))
  (define ref-count (read xref-ip))
  (define obj-locations
    (append
     (sort ; sort by byte offset
      (cdr ; drop zeroth record: there is no zeroth object
       (for/list ([i (in-range ref-count)])
         (cons i (read (open-input-bytes (car (regexp-match #px"\\d{10}" xref-ip)))))))
      < #:key cdr)
     (list (cons #f xoff))))
  (sort ; sort by index
   (parameterize ([current-input-port (open-input-bytes pdf-bs)])
     (for/list ([(idx start) (in-dict obj-locations)]
                [(_ end) (in-dict (cdr obj-locations))])
       (cons idx (parse-pdf-bytes (peek-bytes (- end start) start)))))
   < #:key car))

(define-simple-check (check-pdfs-equal? ps1 ps2)
  (equal? (pdf->dict ps1) (pdf->dict ps2)))


