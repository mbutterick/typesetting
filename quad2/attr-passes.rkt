#lang debug racket/base
(require racket/match
         racket/string
         racket/set
         "attr.rkt"
         "dimension.rkt"
         "pipeline.rkt"
         "struct.rkt"
         "constants.rkt"
         "quad.rkt"
         "param.rkt")
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (for-each-attrs xs proc)
  ;; apply `proc` to each set of attrs in `xs`.
  ;; recursively descend from top to bottom.
  ;; but also track which attrs are visited and skip any already visited.
  (define attrs-seen (mutable-seteq))
  (define wants-parent-attrs? (= (procedure-arity proc) 2))
  (let loop ([xs xs][parent-attrs #false])
    (for ([x (in-list xs)]
          #:when (quad? x))
         (define attrs (quad-attrs x))
         (unless (set-member? attrs-seen attrs)
           (if wants-parent-attrs? (proc attrs parent-attrs) (proc attrs))
           (set-add! attrs-seen attrs))
         (loop (quad-elems x) attrs))))

(define (do-attr-iteration qs
                           #:which-attr [which-attr 'all-attributes-signal]
                           #:attr-proc attr-proc
                           #:wants-parent-attrs [wants-parent-attrs? #false])
  (define attr-predicate
    (match which-attr
      ['all-attributes-signal (λ (ak av) #true)]
      [(? attr-key? attr-key) (λ (ak av) (eq? ak attr-key))]
      [(? procedure? pred)
       (if (eq? 1 (procedure-arity pred))
           (λ (ak _) (pred ak)) ; 1 arity implies key-only test
           pred)]
      [other (raise-argument-error 'do-attr-iteration "key predicate" other)]))
  (for-each-attrs qs
                  (λ (attrs parent-attrs)
                    ;; we don't iterate with `in-hash` (or `in-hash-keys`) because
                    ;; `attrs` might get mutated during the loop,
                    ;; which invalidates the reference `in-hash` is using
                    (for* ([ak (in-list (hash-keys attrs))]
                           [av (in-value (hash-ref attrs ak no-value-signal))]
                           #:when (and (not (eq? av no-value-signal)) (attr-predicate ak av)))
                          (match (if wants-parent-attrs?
                                     (attr-proc ak av attrs parent-attrs)
                                     (attr-proc ak av attrs))
                            ;; void value: do nothing
                            [(? void?) (void)]
                            ;; otherwise treat return value as new attr value
                            [new-av (hash-set! attrs ak new-av)])))))

(define-pass (upgrade-attr-keys qs)
  ;; convert attr keys from symbols to attr struct types
  ;; also lets us validate keys strictly, if we want
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define attr-lookup-table (for/hasheq ([a (in-list (current-attr-keys))])
                                        (values (attr-key-name a) a)))
  (define strict-attrs? (current-strict-attrs?))
  (define (do-upgrade ak av attrs)
    (cond
      [(attr-key? ak) av]
      [(symbol? ak)
       (match (hash-ref attr-lookup-table ak :unknown-key)
         [(== :unknown-key eq?) #:when strict-attrs?
                                (raise-argument-error 'upgrade-attr-keys "known attr" ak)]
         [attr-key
          (hash-remove! attrs ak)
          (hash-set! attrs attr-key av)])]
      [else (raise-argument-error 'upgrade-attr-keys "symbol or attr" ak)]))
  (do-attr-iteration qs #:attr-proc do-upgrade))

(define-pass (set-top-level-attr-values qs)
  ;; put the default values for mandatory keys at the top level
  ;; so that when we linearize, they will percolate downward
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define mandatory-attrs (for/hasheq ([ak (in-list (current-attr-keys))]
                                       #:when (attr-key-mandatory? ak))
                                      (values ak (attr-key-default ak))))
  (list (make-quad #:attrs mandatory-attrs #:elems qs)))

(define-pass (downcase-string-attr-values qs)
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
                     #:attr-proc (λ (ak av attrs)
                                   (match av
                                     [(? boolean?) av]
                                     [(? string? str) #:when (equal? (string-downcase str) "false") #false]
                                     [_ #true]))))

(define-pass (convert-numeric-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-numeric-key?
                     #:attr-proc (λ (ak av attrs)
                                   (or (string->number av)
                                       (raise-argument-error 'convert-numeric-attr-values "numeric string" av)))))

(define-pass (convert-path-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-path-key?
                     #:attr-proc (λ (ak av attrs)
                                   (or (string->path av)
                                       (raise-argument-error 'convert-path-attr-values "path string" av)))))

(define-pass (convert-set-attr-values qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (do-attr-iteration qs
                     #:which-attr attr-set-key?
                     #:attr-proc (λ (ak av attrs)
                                   (apply seteq (map string->symbol (string-split av))))))

(module+ test
  (let ([q (convert-set-attr-values (upgrade-attr-keys (bootstrap-input '(div ((font-features "calt")(font-features-add "")(font-features-subtract "swsh liga"))))))])
    (check-equal? (quad-ref q :font-features) (seteq 'calt))
    (check-equal? (quad-ref q :font-features-add) (seteq))
    (check-equal? (quad-ref q :font-features-subtract) (seteq 'swsh 'liga))))


(define-pass (complete-attr-paths qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; convert every path value to a complete path
  ;; so we don't get tripped up later by relative paths
  ;; relies on `current-directory` being parameterized to source file's dir
  (do-attr-iteration qs
                     #:which-attr attr-path-key?
                     #:attr-proc (λ (ak av attrs)
                                   (unless (path? av)
                                     (raise-argument-error 'complete-attr-paths "path" av))
                                   (path->complete-path av))))


(define-pass (parse-dimension-strings qs)
  #:pre (list-of quad?)
  #:post (list-of quad?)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (do-attr-iteration qs
                     #:which-attr attr-dimension-string-key?
                     #:attr-proc (λ (ak av attrs) (parse-dimension av))))

(module+ test
  (define-attr-list debug-attrs
    [:foo (make-attr-cased-string-key 'foo)]
    [:ps (make-attr-path-key 'ps)]
    [:dim (make-attr-dimension-string-key 'dim)]
    [:boolt (make-attr-boolean-key 'boolt)]
    [:boolf (make-attr-boolean-key 'boolf)]
    [:num (make-attr-numeric-key 'num)]
    [:num-def-42 (make-attr-numeric-key 'num-def-42 #true 42)])
  (parameterize ([current-attr-keys debug-attrs])
    (define (make-q) (make-quad #:attrs (list :foo "BAR"
                                              'ding "dong"
                                              :ps (string->path "file.txt")
                                              :dim "2in"
                                              :boolt "true"
                                              :boolf "false"
                                              :num "42.5")))
    (define qs (list (make-q)))
    (check-exn exn? (λ ()
                      (parameterize ([current-strict-attrs? #true])
                        (upgrade-attr-keys (list (make-q))))))
    (check-not-exn (λ ()
                     (parameterize ([current-strict-attrs? #false])
                       (upgrade-attr-keys (list (make-q))))))
    (check-equal? (quad-ref (car (set-top-level-attr-values (list (make-q)))) :num-def-42) 42)
    (check-equal? (quad-ref (car (downcase-string-attr-values qs)) :foo) "bar")
    (check-true (complete-path? (quad-ref (car (complete-attr-paths qs)) :ps)))
    (check-equal? (quad-ref (car (parse-dimension-strings qs)) :dim) 144)
    (let ([q (car (convert-boolean-attr-values qs))])
      (check-true (quad-ref q :boolt))
      (check-false (quad-ref q :boolf)))
    (check-equal? (quad-ref (car (convert-numeric-attr-values qs)) :num) 42.5)))