#lang at-exp br/quicklang
(require "parser.rkt" "tokenizer.rkt" "struct.rkt" gregor racket/bytes)
(provide (matching-identifiers-out #rx"pf-" (all-defined-out)))

(module+ test (require rackunit))

(module+ reader (provide read-syntax))

(define (read-syntax src port)
  ;; use latin-1 reencoding to make one char = one byte
  (define parse-tree (parse (make-tokenizer (open-input-string (bytes->string/latin-1 (port->bytes port))) src)))
  (strip-bindings
   #`(module pitfall-parse-mod pitfall/parse
       #,parse-tree)))

(define-macro (my-mb ARG ...)
  #'(#%module-begin ARG ...))
(provide (rename-out [my-mb #%module-begin])
         require)

(provide null)

(define-macro (pf-program COS-OBJECT ...)
  #'(begin COS-OBJECT ...))

(define (pf-name str)
  (let* ([str (string-trim str "/" #:right? #f)]
         [str (regexp-replace* @pregexp{#(\d\d)} str (λ (m sub) (string (integer->char (string->number sub 16)))))])
    (string->symbol str)))

(module+ test
  (check-equal? (pf-name "B#45#20NICE") '|BE NICE|))

(define (pf-string arg . tail)
  (cond
    [(andmap byte? (cons arg tail)) (cons arg tail)]
    [(string-prefix? arg "D:")
     #;(parameterize ([current-locale "en"])
         (parse-date "2015-03-15T02:02:02-04:00" "yyyy-MM-dd'T'HH:mm:ssxxx"))
     #f]     
    [else
     (let* ([str (regexp-replace @regexp{^\((.*)\)$} arg "\\1")] ; remove parens
            [str (string-replace str (string-append "\\" "\n") "")]
            [str (regexp-replace* @pregexp{\\(n|r|t|b|f|\(|\)|\\)} str (λ (m sub)
                                                                         (case sub
                                                                           [("n") "\n"]
                                                                           [("r") "\r"]
                                                                           [("t") "\t"]
                                                                           [("b") "\b"]
                                                                           [("f") "\f"]
                                                                           [else sub])))]
            [str (regexp-replace* @pregexp{\\(\d{2,3})} str (λ (m sub) (string (integer->char (string->number sub 8)))))])
       str)]))

(module+ test
  (check-equal? @pf-string{(Testing)} "Testing")
  (check-equal? (pf-string @string-append{(Test\
 ing)}) "Testing")
  (check-equal? @pf-string{(Test\)ing)} "Test)ing")
  (check-equal? @pf-string{(Test\ning)} "Test\ning")
  (check-equal? @pf-string{(Test\\ing)} "Test\\ing")
  (check-equal? @pf-string{(A\53B)} "A+B")
  (check-equal? @pf-string{(A\053B)} "A+B")
  #;(check-equal? @pf-string{(D:19990209153925-08\'00\')})
  #;(check-true (andmap byte? @pf-string{<1C2D3F>}))
  #;(check-true (andmap byte? @pf-string{<1C 2D 3F>})))

(define (pf-array . xs) (co-array xs))

(define (pf-dict . args)
  (co-dict (apply hasheq args)))


(define (pf-stream dict str)
  (define data (string->bytes/utf-8 str))
  (when (not (equal? (hash-ref (co-dict-dict dict) 'Length) (bytes-length data)))
    (raise-argument-error 'pf-stream
                          (format "~a bytes of data" (hash-ref dict 'Length))
                          (format "~a = ~v" (bytes-length data) data)))
  (co-stream dict data))

(define (pf-indirect-object obj gen thing)
  (co-io obj gen thing))

(define-macro (pf-indirect-object-ref (OBJ GEN _))
  #'(co-io-ref OBJ GEN))

(define (pf-header num) (co-header num))

(define (pf-comment text) (co-comment text))
