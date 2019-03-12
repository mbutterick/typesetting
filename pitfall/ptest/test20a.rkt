#lang racket



(require fontland/table/cff/cff-top pitfall/check-pdf racket/dict racket/promise)

(define (dump val)
  (cond
    [(promise? val) 'promise-omitted]
    [(dict? val)
     (for/list ([(k v) (in-dict (sort (dict->list val) #:key car symbol<?))])
       (list k (dump v)))]
    [(list? val) (map dump val)]
    [else val]))

(define misses null)
(define (cmp v1 v2)
  (cond
    [(and (list? v1) (list? v2))
     (and
      (= (length v1) (length v2))
      (for/and ([x1 (in-list v1)]
                [x2 (in-list v2)])
        (unless (cmp x1 x2)
          (set! misses (cons (list v1 x1 v2 x2) misses)))))]
    [else (equal? v1 v2)]))

(define ibs1 (dict-ref (dict-ref (pdf->dict "test20.pdf") 8) 'stream))
(bytes-length ibs1)
(define cfftop1 (dump (send CFFTop decode (open-input-bytes ibs1))))

(define ibs2 (dict-ref (dict-ref (pdf->dict "test20rkt.pdf") 8) 'stream))
(bytes-length ibs2)
(define cfftop2 (dump (send CFFTop decode (open-input-bytes ibs2))))

(cmp cfftop1 cfftop2)
misses