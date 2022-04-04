#lang debug racket/base
(require racket/match
         racket/hash
         racket/list
         racket/string
         "dimension.rkt"
         "pipeline.rkt"
         "quad.rkt")
(provide (all-defined-out))

(define (do-attr-iteration qs #:which-key which-arg #:value-proc proc)
  (define key-predicate
    (match which-arg
      [(? symbol? sym) (λ (k) (eq? k sym))]
      [(and (list (? symbol?) ...) syms) (λ (k) (memq k syms))]
      [(? procedure? pred) pred]
      [other (raise-argument-error 'do-attr-iteration "key predicate" other)]))
  (define attrs-seen (make-hasheq))
  (for ([q (in-list qs)])
       (define attrs (quad-attrs q))
       (hash-ref! attrs-seen attrs
                  (λ ()
                    (for ([k (in-hash-keys attrs)]
                          #:when (key-predicate k))
                         (hash-update! attrs k (λ (val) (proc val attrs))))
                    #t)))
  qs)

;; TODO: make real `has-case-sensitive-value?`
(define (has-case-sensitive-value? x) #false)

(define-pass (downcase-attr-values qs)
  ;; make attribute values lowercase, unless they're case-sensitive
  ;; so we can check them more easily later.
  ;; in principle we could do this earlier and recursively process a single quad
  ;; rather than linearized quads
  ;; it would be faster because there are fewer copies of the attr hashes,
  ;; so we do fewer tests
  ;; but let's stay with the pipeline policy of operating on flat lists of quads
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-key (λ (k) (not (has-case-sensitive-value? k)))
                     #:value-proc (λ (val attrs) (string-downcase val))))

;; TODO: make real `takes-path?`
(define (takes-path? x) (memq x '(ps)))

(define-pass (complete-attr-paths qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; convert every pathlike thing to a complete path (well, path string, because it's inside an attr)
  ;; so we don't get tripped up later by relative paths
  ;; relies on `current-directory` being parameterized to source file's dir
  (do-attr-iteration qs
                     #:which-key takes-path?
                     #:value-proc (λ (val attrs) (path->string (path->complete-path val)))))

;; TODO: make real `takes-dimension-string?`
(define (takes-dimension-string? x) (memq x '(dim)))

(define-pass (parse-dimension-strings qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (do-attr-iteration qs
                     #:which-key takes-dimension-string?
                     #:value-proc (λ (val attrs) (parse-dimension val attrs))))

(module+ test
  (require rackunit)
  (define q (make-quad #:attrs (make-hasheq '((foo . "BAR")(ps . "file.txt")(dim . "2in")))))
  (define qs (list q))
  (check-equal? (quad-ref (car (downcase-attr-values qs)) 'foo) "bar")
  (check-true (complete-path? (string->path (quad-ref (car (complete-attr-paths qs)) 'ps))))
  (check-equal? (quad-ref (car (parse-dimension-strings qs)) 'dim) 144))