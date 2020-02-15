#lang debug racket
(require quad/base "struct.rkt" "param.rkt")
(provide (all-defined-out))

(define (make-query-index q)
  (define qs (let loop ([q q])
               (cons q (append* (for/list ([elem (in-list (quad-elems q))]
                                           #:when (quad? elem))
                                  (loop elem))))))
  (list->vector qs))

(define (string->key str)
  (match str
    ["doc" 'doc]
    [(or "section" "sec" "s") 'section]
    [(or "page" "pg" "p") 'page]
    [(or "column" "col" "c") 'column]
    [(or "block" "b") 'block]
    [(or "line" "ln" "l") 'line]))

(define preds (hasheq 'doc doc-quad?
                      'section section-quad?
                      'page page-quad?
                      'column column-quad?
                      'block block-quad?
                      'line line-quad?))

(define (parse-query str)
  (for/list ([piece (in-list (string-split str ":"))])
    (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
      [#false (cons (string->key piece) #false)]
      [(list _ name arg) (cons (hash-ref preds (string->key name))
                               (or (string->number arg) (string->symbol arg)))])))

(define (find-inclusive vec pred start-arg end-arg [count 1])
  ;; search from lidx to ridx inclusive
  (define-values (start end step)
    (cond
      ;; if lidx is bigger, search backward
      [(> start-arg end-arg) (values start-arg (sub1 end-arg) -1)]
      [else (values start-arg (add1 end-arg) 1)]))
  (for/fold ([start-adjusted (- start step)]) ; remove step for reason below
            ([seen (in-range count)]
             #:break (not start-adjusted))
    ;; add step so we find next matcher
    ;; and won't re-find the last one immediately
    (for/first ([idx (in-range (+ start-adjusted step) end step)]
                #:when (pred (vector-ref vec idx)))
      idx)))

(define (query quad-or-index query-str [starting-q #false])
  (define vec (match quad-or-index
                [(? quad? q) (make-query-index q)]
                [idx idx]))
  (for/fold ([vidx 0]
             [maxidx (sub1 (vector-length vec))]
             #:result (and vidx (vector-ref vec vidx)))
            ([query-piece (in-list (parse-query query-str))]
             #:break (not vidx))
    (match-define (cons pred count) query-piece)
    (define res-idx
      (match count
        [(== 'this eq?) ; start at querying quad, then search backward
         (find-inclusive vec pred (vector-memq starting-q vec) 0)]
        [(== 'last eq?) ; search backward from end
         (find-inclusive vec pred maxidx 0)]
        [(== 'prev eq?) (find-inclusive vec pred (vector-memq starting-q vec) 0 2)]
        [(== 'next eq?) (find-inclusive vec pred ((if (pred starting-q) add1 values) (vector-memq starting-q vec)) maxidx)]
        [count (find-inclusive vec pred vidx maxidx count)]))
    (define next-maxidx
      (cond
        [(not res-idx) maxidx]
        ;; try searching for next occurence after this one, up to max.
        [(find-inclusive vec pred (add1 res-idx) maxidx)]
        [else maxidx]))
    (values res-idx next-maxidx)))
          
(module+ test
  (require rackunit)
  
  (define counter 0)
  (define-syntax-rule (factory type proc)
    (make-quad #:type type
               #:elems (for/list ([i (in-range 3)])
                         (set! counter (add1 counter))
                         (define new-q (proc))
                         (quad-update! new-q
                                       [tag (format "~a[~a]-~a" 'proc counter (gensym))])
                         (hash-set! (quad-attrs new-q) 'count counter)
                         new-q)))

  (define (line) (make-quad #:type line-quad))
  (define (block) (factory block-quad line))
  (define (col) (factory column-quad block))
  (define (page) (factory page-quad col))
  (define (sec) (factory section-quad page))

  (define (count q) (and q (quad-ref q 'count)))
  (define doc (factory doc-quad sec))
  
  (check-equal? (count (query doc "sec[2]")) 242)
  (check-false (query doc "sec[102]:line[1]"))
  (check-equal? (count (query doc "sec[2]:pg[1]")) 162)
  (check-equal? (count (query doc "sec[2]:pg[1]:ln[3]")) 128)
  (check-eq? (query doc "page[this]" (query doc "line[2]")) (query doc "page[1]"))
  (check-equal? (count (query doc "page[this]:line[last]" (query doc "line[2]"))) 41)
 
  (check-equal? (count (query doc "sec[next]" (query doc "sec[1]"))) (count (query doc "sec[2]")) )
  (check-equal? (count (query doc "sec[prev]" (query doc "sec[2]"))) (count (query doc "sec[1]")))

  (check-equal? (count (query doc "page[prev]" (query doc "page[2]:line[1]")))
                (count (query doc "page[1]")))
  (check-equal? (count (query doc "page[next]" (query doc "page[2]:line[1]")))
                (count (query doc "page[3]")))
  
  )