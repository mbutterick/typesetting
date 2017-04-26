#lang at-exp br
(require racket/string pitfall/struct pitfall/measure br/define racket/bytes sugar/debug racket/format racket/file)
(provide (all-defined-out)
         (all-from-out pitfall/struct pitfall/measure)
         file->bytes
         (except-out (all-from-out br) #%module-begin))

(define-macro (mb . ARGS)
  #'(#%module-begin (render-args . ARGS)))
(provide (rename-out [mb #%module-begin]))

(define (make-xref-table locs)
  (string->bytes/latin-1 @string-append{
 xref
 0 @(number->string (length locs))
 0000000000 65535 f
 @(let ([sep " 00000 n\n"])
    (string-join
     (for/list ([loc (in-list (cdr (sort locs < #:key car)))])
       (~r #:min-width 10 #:pad-string "0" (cdr loc))) sep #:after-last sep))
 }))

(define (render-args . args)
  (render-list args))

(define (render-list args)
  (define-values (bstrs offset locs)
    (for/fold ([cobstrs null]
               [offset 0]
               [io-locs '((0 . 0))])
              ([cosexpr (in-list args)])
      (define cobstr (cosexpr->bytes cosexpr))
      (values
       (cons cobstr cobstrs)
       (+ offset (bytes-length cobstr))
       (if (co-io? cosexpr)
           (cons (cons (co-io-idx cosexpr) offset) io-locs)
           io-locs))))
  (define header-str (cosexpr->bytes (co-header "%PDF-1.6\n%Â¥Â±Ã«")))
  (define trailer-str (cosexpr->bytes
                       (co-trailer (co-dict (hasheq 'Size (length bstrs) 'Root (co-io-ref 1 0))))))
  (define last-offset (for/sum ([bstr (in-list bstrs)])
                        (bytes-length bstr)))
  (define result (apply bytes-append `(,header-str
                                       ,@(reverse bstrs)
                                       ,(make-xref-table locs)
                                       ,trailer-str
                                       #"\nstartxref\n"
                                       ,(string->bytes/latin-1 (number->string last-offset))
                                       #"\n%%EOF")))
  #;(display result)
  (let ([op (open-output-file (expand-user-path "~/Desktop/foo.pdf") #:exists 'replace)])
    (write-bytes result op)
    (close-output-port op))
  #;result)
  
(define (cosexpr->bytes x)
  (bytes-append
   (string->bytes/latin-1
    (let loop ([x x])
      (cond
        [(co-version? x)
         @string-append{%%PDF-@(number->string (co-version-num x))}]
        [(co-header? x) (loop (co-header-string x))]
        [(co-array? x)
         @string-append{[ @(string-join (map loop (co-array-items x)) " ") ]}]
        [(co-io? x)
         @string-append{
          @(loop (co-io-idx x)) @(loop (co-io-rev x)) obj
          @(loop (co-io-thing x))
          endobj

          }]
        [(co-dict? x)
         @string-append{
          <<
          @(string-join
            (for/list ([(k v) (in-hash (co-dict-dict x))])
              @string-append{@(loop k) @(loop v)}) "\n")
          >>}]
        [(co-io-ref? x)
         @string-append{@(loop (co-io-ref-idx x)) @(loop (co-io-ref-rev x)) R}]
        [(co-string? x) (format "(~a)" (co-string-string x))]
        [(co-stream? x)
         @string-append{
          @(loop (co-stream-dict x))
          stream
          @(loop (co-stream-data x))
          endstream
          }]
        #;[(co-comment? x) (co-comment-text x)]
        [(co-trailer? x) @string-append{
          trailer
          @(loop (co-trailer-dict x))
          }]
        [(symbol? x) @string-append{/@(symbol->string x)}]
        [(number? x) @number->string{@x}]
        [(string? x) x]
        [else (format "~a" x)]))) #"\n"))


(define (co-catalog #:pages io-ref)
  (co-dict (hasheq 'Type 'Catalog 'Pages io-ref)))

(define (co-pages #:kids kidslist #:count count)
  (co-dict (hasheq 'Type 'Pages
                   'Kids (co-array kidslist)
                   'Count count)))

(define (co-page #:parent parent
                 #:mediabox pts
                 #:resources [rsrcs (co-dict (hasheq))]
                 #:contents contents
                 #:annots [annots (co-array null)])
  (co-dict (hasheq 'Type 'Page
                   'Parent parent
                   'MediaBox (co-array pts)
                   'Resources rsrcs
                   'Contents contents
                   'Annots annots)))

(define (make-co-dict . xs)
  (co-dict (apply hasheq xs)))

(define (make-co-stream bstr . kvs)
  (co-stream  (apply make-co-dict 'Length (bytes-length bstr) kvs) (bytes->string/latin-1 bstr)))

(define (make-font-co-stream font-path)
  (make-co-stream (file->bytes font-path) 'Subtype 'OpenType))

#;(cosexpr->bytes (make-co-dict 'Hello (co-string "World")))