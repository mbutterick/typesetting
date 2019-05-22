#lang debug racket
(require rackunit (prefix-in zlib: fontland/zlib) fontland/table/cff/cff-top)
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

(define (between-delimiters bs left right)
  (parameterize ([current-input-port (if (input-port? bs) bs (open-input-bytes bs))])
    (let loop ([acc null][depth 0])
      (cond
        [(regexp-try-match (pregexp (string-append "^" (regexp-quote (~a left)))) (current-input-port))
         => (λ (m)
              (loop (if (empty? acc)
                        acc ; impliedly throw away first left delimiter
                        (cons (car m) acc)) (add1 depth)))]
        [(regexp-try-match (pregexp (string-append "^" (regexp-quote (~a right)))) (current-input-port)) 
         => (λ (m)
              (case depth
                [(1) (bytes-append* (reverse acc))]
                [else (loop (cons (car m) acc) (sub1 depth))]))]
        [else
         (define bstr (read-byte))
         (and (not (eof-object? bstr))
              (loop (if (zero? depth)
                        acc ; impliedly throw away leading non-delimiter bytes
                        (cons (bytes bstr) acc)) depth))]))))

(module+ test
  (require rackunit)
  (define bs #"a<<b<<c>>x<<z>>d>>e<<f>>g")
  (check-equal? (between-delimiters bs #"<<" #">>") #"b<<c>>x<<z>>d")
  (check-equal? (between-delimiters (between-delimiters bs #"<<" #">>") #"<<" #">>") #"c")
  (check-false (between-delimiters #"abc" #"<<" #">>"))
  (check-equal? (between-delimiters #"[a[b]c]" #"[" #"]") #"a[b]c")
  (check-equal? (let ([ip (open-input-bytes #"<</foo 42>>z")])
                  (parse-1 ip)
                  (port->bytes ip)) #"z"))

(define (parse-1 ip)
  (cond
    ;; the complication is that arrays & dicts can contain other arrays & dicts
    ;; so we have to scan ahead in an intelligent way.
    [(equal? (peek-bytes 1 0 ip) #"[") ;; array
     (parse-pdf-bytes (between-delimiters ip #"[" #"]"))]
    [(equal? (peek-bytes 2 0 ip) #"<<") ;; dict, maybe with stream
     (define items (parse-pdf-bytes (between-delimiters ip #"<<" #">>")))
     (unless (even? (length items)) (raise items))
     (define dic
       (sort ; put hash into order so it's comparable
        (for/list ([kv (in-slice 2 items)]
                   ;; suppress these keys so we can compare pdfkit & pitfall output
                   #:unless (member (car kv) (list #"/Producer" #"/Creator" #"/CreationDate")))
                  (apply cons kv))
        bytes<?
        #:key car))
     (cond ;; might have a stream
       [(regexp-try-match #px"^\\s*stream\n" ip)
        (define stream-length
          (read (open-input-bytes (cdr (assoc #"/Length" dic)))))
        (define compressed? (equal? (dict-ref dic #"/Filter" #f) #"/FlateDecode"))
        (define stream ((if compressed? zlib:inflate values) (read-bytes stream-length ip)))
        ;; font subsets have their own interior structure, so ignore (maybe too lenient)
        (define font? (equal? (subbytes stream 0 4) #"true"))
        (dict-update
         (append dic
                 (list (cons 'stream (if font? #"0" stream))))
         ;; compressed length may vary, so just set to #"0"
         #"/Length" (λ (val) (cond
                               [(or font? compressed?) (bytes-length stream)]
                               [else val])))]
       [else dic])]
    [else
     (pat-lex ip
              ["\\s+" (parse-1 ip)] ; whitespace
              ["\\d+ 0 obj" (parse-1 ip)] ;; obj name
              ["\\d+ 0 R"] ; xref
              ["[-]?\\d*\\.\\d+"] ; real
              ["[-]?\\d+\\.?"] ; integer
              ["\\(.*?\\)"] ; parenstring
              ["/[A-Z]{6}(\\+\\S+)" => cadr] ; font keystring. prefix is random, so ignore
              ["/\\S+"] ; keystring
              [else eof])]))

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
               (cons idx (car (parse-pdf-bytes (peek-bytes (- end start) start))))))
   < #:key car))

(define (dict-compare arg1 arg2)
  (define d1 (if (dict? arg1) arg1 (pdf->dict arg1)))
  (define d2 (if (dict? arg2) arg2 (pdf->dict arg2)))
  (and (dict? d1) (dict? d2)
       (= (length d1) (length d2))
       (for/and ([(k1 v1) (in-dict d1)]
                 [(k2 v2) (in-dict d2)])
                (unless (equal? k1 k2)
                  (error (format "keys unequal in ~a and ~a: ~a ≠ ~a" arg1 arg2 k1 k2)))
                (unless (equal? v1 v2)
                  (error (format "values unequal in ~a and ~a: ~e ≠ ~e" arg1 arg2 v1 v2)))
                (when (dict? v1)
                  (dict-compare v1 v2))
                #true)))

(define-simple-check (check-headers-equal? ps1 ps2)
  (equal? (peek-bytes 14 0 (open-input-file ps1))
          (peek-bytes 14 0 (open-input-file ps2))))

(define-simple-check (check-pdfs-equal? ps1 ps2)
  (dict-compare ps1 ps2))

(define-simple-check (check-font-subsets-equal? f1 f2)
  (define misses null)

  (define (dump val)
    (cond
      [(promise? val) 'promise-omitted]
      [(vector? val) (dump (vector->list val))]
      [(dict? val)
       (for/list ([(k v) (in-dict (sort (dict->list val) #:key car symbol<?))])
                 (list k (dump v)))]
      [(list? val) (map dump val)]
      [else val]))

  (define (cmp v1 v2)
    (cond
      [(and (list? v1) (list? v2))
       (and
        (= (length v1) (length v2))
        (for/and ([x1 (in-list v1)]
                  [x2 (in-list v2)])
                 (unless (cmp x1 x2)
                   (set! misses (cons (list v1 x1 v2 x2) misses)))))]
      [else (equal? v1 v2)]))

  (define ibs1 (dict-ref (dict-ref (pdf->dict f1) 8) 'stream))
  (define cfftop1 (dump (send CFFTop x:decode (open-input-bytes ibs1))))
  (define ibs2 (dict-ref (dict-ref (pdf->dict f2) 8) 'stream))
  (define cfftop2 (dump (send CFFTop x:decode (open-input-bytes ibs2))))

  (cmp cfftop1 cfftop2)
  (check-true (null? misses)))

#;(module+ main
    (for ([p (in-directory)]
          #:when (path-has-extension? p #"pdf"))
         (with-handlers ([exn:fail? (λ (exn) (println (format "~a failed" p)))])
           (pdf->dict p))))
