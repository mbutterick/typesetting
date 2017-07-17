#lang br
(require racket/file sugar/cache sugar/port racket/dict)

(define dict-key-pattern #px"^/\\S+")

(struct ref (id) #:transparent)

(define (trim-whitespace bs) ; can't use string-trim with bytes
  (cadr (regexp-match #px"^\\s*(.*?)\\s*$" bs))) 

(define (parse-dict p)
  (define terms (for/list ([next (in-port parse-one p)]
                           #:break (eq? next 'end-dict))
                          next))
  (for/list ([k (in-list terms)]
             [v (in-list (cdr terms))]
             [i (in-naturals)]
             #:when (even? i))
            (cons k v)))

(define (parse-list p)
  (for/list ([next (in-port parse-one p)]
             #:break (eq? next 'end-list))
            next))

(define (parse-one p)
  (let loop ([p-in p])
    (define p (if (input-port? p-in) p-in  (open-input-bytes p-in)))
    (cond
      ;; skip whitespace
      [(regexp-try-match #px"^\\s+" p) (loop p)]
      ;; indirect reference => reference int
      [(regexp-try-match #px"^(\\d+) (\\d+) R" p) => (λ (m) (string->symbol (format "ref-~a" (loop (cadr m)))))]
      ;; object => trim and reparse
      [(regexp-try-match #px"^\\d+ 0 obj(.*?)endobj" p) => (λ (m) (loop (cadr m)))]
      ;; integer string => int
      [(regexp-try-match #px"^\\d+" p) => (λ (m) (string->number (bytes->string/utf-8 (car m))))]
      ;; dict => dispatch to handler
      [(regexp-try-match #rx"^<<" p) (parse-dict p)]
      [(regexp-try-match #rx"^>>" p) 'end-dict]
      ;; list => unroll
      [(regexp-try-match #rx"^\\[" p) (parse-list p)]
      [(regexp-try-match #rx"^]" p) 'end-list]
      ;; slash-prefixed dictionary key => symbol
      [(regexp-try-match dict-key-pattern p) => (λ (m) (string->symbol (string-trim (bytes->string/utf-8 (car m)) "/" #:right? #f)))]
      [(regexp-try-match #rx"^startxref" p) (loop p)] ; pulls off next item, which is an integer
      [(regexp-try-match #rx"^<([0123456789abcdef]*)>" p) => (λ (m) (cadr m))]
      [(eof-object? (peek-bytes 1 0 p)) eof]
      [else (report* (file-position p) (peek-bytes 3 0 p)) (error 'unknown-type)])))

(struct trailer (dict xref-ptr) #:transparent)
(define (parse-trailer p)
  (define eof-position (file-size (object-name p)))
  (define trailer-pos (for*/first ([pos (in-range eof-position 0 -1)]
                                   [m (in-value (regexp-try-match "^\ntrailer\n.*" p pos))]
                                   #:when m)
                                  pos))
  (define dict-offset (caar (regexp-match-positions #rx"<<.*?>>" (port-position p trailer-pos))))
  (define trailer-dict (parse-one (port-position p (+ trailer-pos dict-offset))))
  (define xref-ptr (parse-one p))
  (trailer trailer-dict xref-ptr))


(define (parse-xref p offset)
  (define bs (car (regexp-match #rx"(?<=xref).*?(?=trailer)" (port-position p offset))))
  (define str (bytes->string/utf-8 bs))
  (for/hash ([k (in-naturals 1)]
             [v (in-list (drop (string-split str "\n") 2))])
            (values k (string->number (car (regexp-match #px"^\\d+" v))))))

(struct $pdf (port xrefs) #:transparent)
(define/caching (doc-ref pdf idx)
  (define offset (hash-ref ($pdf-xrefs pdf) idx))
  (parse-one (port-position ($pdf-port pdf) offset)))

(define/caching (doc-ref* pdf idx)
  ;; resolve nested ref-x pointers
  (define visited empty) ; prevent circular references by tracking visited refs
  (let loop ([result (doc-ref pdf idx)])
    (cond
      [(dict? result) (for/list ([(k v) (in-dict result)])
                                (cons k (loop v)))]
      [(list? result) (map loop result)]
      [(and (symbol? result) (regexp-match #px"^ref-(\\d+)" (symbol->string result)))
       => (λ (m) (define next-idx (string->number (cadr m)))
            (define old-visited visited)
            (set! visited (cons next-idx visited))
            (if (member next-idx old-visited)
                result
                (loop (doc-ref pdf next-idx))))]
      [else result])))


(define (port->pdf p)
  ($pdf p (parse-xref p (trailer-xref-ptr (parse-trailer p)))))

(module+ test
  (require rackunit)
  (define p (open-input-file "test/minimal.pdf"))
  (define pdf (port->pdf p))
  (check-equal? (doc-ref pdf 1) '((Type . Catalog) (Pages . ref-2)))
  (check-equal? (doc-ref pdf 2) '((Type . Pages) (Kids ref-3) (Count . 1) (MediaBox 0 0 300 144)))
  (check-equal? (doc-ref pdf 3) '((Type . Page) (Parent . ref-2) (Resources (Font (F1 (Type . Font) (Subtype . Type1) (BaseFont . Times-Roman)))) (Contents . ref-4)))
  (check-equal? (doc-ref pdf 4) '((Length . 55))))


(define p (open-input-file "test/test-cff/textedit-sample.pdf"))
(define pdf (port->pdf p))
