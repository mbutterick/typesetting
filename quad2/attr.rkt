#lang debug racket/base
(require racket/match
         racket/hash
         racket/list
         racket/string
         racket/set
         "dimension.rkt"
         "pipeline.rkt"
         "struct.rkt"
         "constants.rkt"
         "quad.rkt"
         "param.rkt")
(provide (all-defined-out))

(define (do-attr-iteration qs
                           #:which-attr which-attr
                           #:attr-proc attr-proc)
  (define attr-predicate
    (match which-attr
      [(? attr-key? attr-key) (λ (ak av) (eq? ak attr-key))]
      [(? procedure? pred) (if (eq? 1 (procedure-arity pred))
                               (λ (ak _) (pred ak)) ; 1 arity implies key-only test
                               pred)]
      [other (raise-argument-error 'do-attr-iteration "key predicate" other)]))
  (define attrs-seen (mutable-seteq))
  (let loop ([xs qs])
    (for ([x (in-list xs)]
          #:when (quad? x))
         (let* ([q x]
                [attrs (quad-attrs q)])
           (unless (set-member? attrs-seen attrs)
             (for ([(ak av) (in-hash attrs)]
                   #:when (attr-predicate ak av))
                  (hash-set! attrs ak (attr-proc ak av attrs)))
             (set-add! attrs-seen attrs))
           (loop (quad-elems q)))))
  qs)

(define-pass (upgrade-attr-keys qs)
  ;; convert attr keys from symbols to attr struct types
  ;; also lets us validate keys strictly, if we want
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define attr-lookup-table (for/hasheq ([a (in-list (current-attrs))])
                                        (values (attr-key-name a) a)))
  (define attrs-seen (mutable-seteq))
  (define strict-attrs? (current-strict-attrs?))
  (define (do-upgrade ak av attrs)
    (cond
      [(symbol? ak)
       (match (hash-ref attr-lookup-table ak #false)
         [(? attr-key? attr)
          (hash-remove! attrs ak)
          (hash-set! attrs attr av)]
         [_ #:when strict-attrs?
            (raise-argument-error 'upgrade-attr-keys "known attr" ak)]
         [_ (void)])]
      [else (raise-argument-error 'upgrade-attr-keys "symbol or attr" ak)]))
  (do-attr-iteration qs
                     #:which-attr (λ (ak) (not (attr-key? ak)))
                     #:attr-proc do-upgrade))

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
                     #:which-attr attr-cased-string-key?
                     #:attr-proc (λ (ak av attrs) (string-downcase av))))


(define-pass (convert-boolean-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-boolean-key?
                     #:attr-proc (λ (ak av attrs) (match (string-downcase av)
                                                     ["false" #false]
                                                     [_ #true]))))

(define-pass (convert-numeric-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-numeric-key?
                     #:attr-proc (λ (ak av attrs)
                                    (cond
                                      [(string->number av)]
                                      [else
                                       (raise-argument-error 'convert-numeric-attr-values "numeric string" av)]))))

(define-pass (complete-attr-paths qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; convert every pathlike thing to a complete path (well, path string, because it's inside an attr)
  ;; so we don't get tripped up later by relative paths
  ;; relies on `current-directory` being parameterized to source file's dir
  (do-attr-iteration qs
                     #:which-attr attr-path-key?
                     #:attr-proc (λ (ak av attrs) (path->complete-path av))))

(define-pass (parse-dimension-strings qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (do-attr-iteration qs
                     #:which-attr attr-dimension-string-key?
                     #:attr-proc (λ (ak av attrs) (parse-dimension av attrs))))

(module+ test
  (require rackunit)
  (define-attr-list debug-attrs
    [:foo (attr-cased-string-key 'foo)]
    [:ps (attr-path-key 'ps)]
    [:dim (attr-dimension-string-key 'dim)]
    [:boolt (attr-boolean-key 'bool)]
    [:boolf (attr-boolean-key 'bool)]
    [:num (attr-numeric-key 'num)])
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
    (check-true (complete-path? (quad-ref (car (complete-attr-paths qs)) :ps)))
    (check-equal? (quad-ref (car (parse-dimension-strings qs)) :dim) 144)
    (let ([q (car (convert-boolean-attr-values qs))])
      (check-true (quad-ref q :boolt))
      (check-false (quad-ref q :boolf)))
    (check-equal? (quad-ref (car (convert-numeric-attr-values qs)) :num) 42.5)))