#lang debug racket/base
(require racket/list
         racket/match
         quadwriter/core
         "tags.rkt"
         "lang-helper.rkt")
(provide (all-defined-out)
         #%app #%top #%datum #%top-interaction
         (all-from-out "tags.rkt"))

(define rsquo "’")
(define rdquo "”")
(define lsquo "‘")
(define ldquo "“")
(define hellip "…")
(define ndash "–")
(define mdash "—")

(define (doc-proc exprs)
  (define strs (match exprs
                 [(? null?) '(" ")] ; single nonbreaking space, so something prints
                 [strs strs]))
  ;; markdown parser returns list of paragraphs
  (root null (add-between strs (list pbr)
                          #:before-first (list pbr)
                          #:after-last (list pbr)
                          #:splice? #true)))
(make-module-begin doc-proc)

(module reader racket/base
  (require racket/port markdown "lang-helper.rkt")
  (provide read-syntax)
  (define read-syntax (make-read-syntax 'quadwriter/markdown
                       (λ (path-string p) (xexpr->parse-tree (parse-markdown (port->string p)))))))