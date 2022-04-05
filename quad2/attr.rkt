#lang debug racket/base
(require racket/match
         racket/hash
         racket/list
         racket/string
         "dimension.rkt"
         "pipeline.rkt"
         "struct.rkt"
         "constants.rkt"
         "quad.rkt"
         "param.rkt")
(provide (all-defined-out))

(define (do-attr-iteration qs #:which-attr which-arg #:value-proc proc)
  (define key-predicate
    (match which-arg
      [(? attr? attr) (λ (k) (eq? k (attr-name attr)))]
      [(and (list (? attr?) ...) attrs) (λ (k) (memq k (map attr-name attrs)))]
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

(define-pass (upgrade-attr-keys qs)
  ;; convert attr keys from symbols to attr struct types
  ;; also lets us validate keys strictly, if we want
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define attr-lookup-table (for/hasheq ([a (in-list (current-attrs))])
                                        (values (attr-name a) a)))
  (define attrs-seen (make-hasheq))
  (define strict-attrs? (current-strict-attrs?))
  (for ([q (in-list qs)])
       (define attrs (quad-attrs q))
       (hash-ref! attrs-seen attrs
                  (λ ()
                    (for ([(k v) (in-hash attrs)]
                          #:unless (attr? k))
                         (cond
                           [(symbol? k)
                            (match (hash-ref attr-lookup-table k #false)
                              [(? attr? attr)
                               (hash-remove! attrs k)
                               (hash-set! attrs attr v)]
                              [_ #:when strict-attrs?
                                 (raise-argument-error 'upgrade-attr-keys "known attr" k)]
                              [_ (void)])]
                           [else (raise-argument-error 'upgrade-attr-keys "symbol or attr" k)]))
                    #t)))
  qs)

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
                     #:which-attr attr-cased-string?
                     #:value-proc (λ (val attrs) (string-downcase val))))


(define-pass (convert-boolean-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-boolean?
                     #:value-proc (λ (val attrs) (match (string-downcase val)
                                                   ["false" #false]
                                                   [_ #true]))))

(define-pass (convert-numeric-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-numeric?
                     #:value-proc (λ (val attrs)
                                    (or (string->number val)
                                        (raise-argument-error 'convert-numeric-attr-values "numeric string" val)))))

(define-pass (complete-attr-paths qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; convert every pathlike thing to a complete path (well, path string, because it's inside an attr)
  ;; so we don't get tripped up later by relative paths
  ;; relies on `current-directory` being parameterized to source file's dir
  (do-attr-iteration qs
                     #:which-attr attr-path?
                     #:value-proc (λ (val attrs) (path->complete-path val))))

(define-pass (parse-dimension-strings qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (do-attr-iteration qs
                     #:which-attr attr-dimension-string?
                     #:value-proc parse-dimension))

(module+ test
  (require rackunit)
  (define-attr-list debug-attrs
    [:foo (attr-cased-string 'foo)]
    [:ps (attr-path 'ps)]
    [:dim (attr-dimension-string 'dim)]
    [:boolt (attr-boolean 'bool)]
    [:boolf (attr-boolean 'bool)]
    [:num (attr-numeric 'num)])
  (parameterize ([current-attrs debug-attrs])
    (define q (make-quad #:attrs (make-hasheq (list (cons :foo "BAR")
                                                    (cons 'ding "dong")
                                                    (cons :ps "file.txt")
                                                    (cons :dim "2in")
                                                    (cons :boolt "true")
                                                    (cons :boolf "false")
                                                    (cons :num "42.5")))))
    (define qs (list q))
    (check-not-exn (λ ()
                     (parameterize ([current-strict-attrs? #false])
                       (upgrade-attr-keys qs))))
    (check-exn exn? (λ ()
                      (parameterize ([current-strict-attrs? #true])
                        (upgrade-attr-keys qs))))
    (check-equal? (quad-ref (car (downcase-attr-values qs)) :foo) "bar")
    (check-true (complete-path? (string->path (quad-ref (car (complete-attr-paths qs)) :ps))))
    (check-equal? (quad-ref (car (parse-dimension-strings qs)) :dim) 144)
    (let ([q (car (convert-boolean-attr-values qs))])
      (check-true (quad-ref q :boolt))
      (check-false (quad-ref q :boolf)))
    (check-equal? (quad-ref (car (convert-numeric-attr-values qs)) :num) 42.5)))