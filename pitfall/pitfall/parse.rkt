#lang at-exp br/quicklang
(require "parser.rkt" "tokenizer.rkt" gregor)
(provide (matching-identifiers-out #rx"pf-" (all-defined-out)))

(module+ test (require rackunit))

(module+ reader (provide read-syntax))

(define (read-syntax src port)
  (define parse-tree (parse (make-tokenizer src port)))
  (strip-bindings
   #`(module pitfall-parse-mod pitfall/parse
       #,parse-tree)))

(define-macro (my-mb PARSE-TREE)
  #'(#%module-begin PARSE-TREE))
(provide (rename-out [my-mb #%module-begin]))

(provide null)

(define-macro (pf-program ARG ...)
  #'(list ARG ...))

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
     (let* ([str (regexp-replace @regexp{^\((.*)\)$} arg "\\1")]
            [str (regexp-replace* @pregexp{\\(\\|\))} str "\\1")]
           [str (regexp-replace* @pregexp{\\(\d\d\d)} str (λ (m sub) (string (integer->char (string->number sub)))))])
       str)]))

(module+ test
  (check-equal? (pf-string "(Testing)") "Testing")
  (check-equal? (pf-string "(Test\\)ing)") "Test)ing")
  (check-equal? (pf-string "(Test\\\\ing)") "Test\\ing")
  (check-equal? (pf-string "(A\\043B)") "A+B")
  #;(check-equal? (pf-string "(D:19990209153925-08\'00\')") )
  #;(check-true (andmap byte? (pf-string "<1C2D3F>")))
  #;(check-true (andmap byte? (pf-string "<1C 2D 3F>"))))

(define (pf-array . xs) xs)

(define (pf-dict . args)
  (apply hash args))
      