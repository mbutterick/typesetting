#lang debug racket
(require racket/contract racket/list racket/match txexpr sugar/debug sugar/define sugar/list racket/promise racket/function (only-in racket/control call/prompt) racket/future 
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt" "generic.rkt" "position.rkt")

(define-syntax (debug-report stx)
  (syntax-case stx ()
    [(_ EXPR ...) (with-syntax ([debug (datum->syntax stx 'debug)])
                    #'(when debug (report EXPR ...)))]))

(define distance-cache (make-hasheq))
(define/contract (distance q)
  (any/c . -> . real?)
  (hash-ref! distance-cache (cond
                              [(quad? q)
                                (hash-ref (attrs q) 'id q)]
                              [(symbol? q) q])
             (λ ()
               (cond
                 [(quad? q)
                  (match-define (list ∆x ∆y) (map - (out-point q) (in-point q)))
                  (cond
                    [(zero? ∆x) ∆y]
                    [(zero? ∆y) ∆x]
                    [else (sqrt (+ (* ∆x ∆x) (* ∆y ∆y)))])]
                 [else 0]))))


(define+provide/contract (break xs
                                [target-size (current-wrap-distance)]
                                [debug #f]
                                #:break-val [break-val 'break]
                                #:break-before? [break-before? #f]
                                #:break-after? [break-after? #f]
                                #:hard-break-proc [hard-break? (const #f)]
                                #:soft-break-proc [soft-break? (const #f)]
                                #:finish-wrap-proc [finish-wrap-proc values])
  ((any/c) (real? any/c
                  #:break-val any/c
                  #:break-before? boolean?
                  #:break-after? boolean?
                  #:hard-break-proc procedure?
                  #:soft-break-proc procedure?
                  #:finish-wrap-proc procedure?) . ->* . (listof any/c))
  (break-hards xs
               target-size
               debug
               break-val
               break-before?
               break-after?
               hard-break?
               soft-break?
               finish-wrap-proc))

;; the hard breaks are used to divide the wrap territory into smaller chunks
;; that can be cached, parallelized, etc.
(define (break-hards xs
                     target-size
                     debug
                     break-val
                     break-before?
                     break-after?
                     hard-break?
                     soft-break?
                     finish-wrap-proc)
  (define break-val=? (if (symbol? break-val) eq? equal?))
  (define (cleanup-wraplist xs)
    (dropf-right (append* (reverse xs)) (λ (x) (break-val=? break-val x))))
  (define wraps
    (for/fold ([wraps null]
               [xs xs]
               #:result wraps)
              ([i (in-naturals)]
               #:break (null? xs))
      (match xs
        [(cons (? hard-break?) rest)
         (debug-report x 'hard-break)
         (values (cons (list break-val) wraps) rest)]
        [_ (define-values (head tail) (splitf-at xs (λ (x) (not (hard-break? x)))))
           (values (cons (cleanup-wraplist (break-softs head
                                                        target-size
                                                        debug
                                                        break-val
                                                        soft-break?
                                                        finish-wrap-proc)) wraps) tail)])))
  (append (if break-before? (list break-val) empty)
          (cleanup-wraplist wraps)
          (if break-after? (list break-val) empty)))

(define (nonprinting-at-start? x) (if (quad? x) (not (printable? x 'start)) #t))
(define (nonprinting-at-end? x) (if (quad? x) (not (printable? x 'end)) #t))
(define (nonprinting-in-middle-soft-break? x) (and (quad? x) (not (printable? x)) (soft-break? x)))

(define (append-to-wrap partial wrap)
  (match/values
   (values partial wrap)
   [((== empty) _) wrap]
   [(partial (list (? nonprinting-in-middle-soft-break?) ... rest ...)) (append (or partial null) rest)]))

(define (break-softs qs
                     target-size
                     debug
                     break-val
                     soft-break?
                     finish-wrap-proc)
  (define start-signal (gensym))
  ;; qs = list of quads
  ;; current-dist = integer
  ;; current-wrap = list of quads ending in previous `soft-break?`
  ;; current-partial = list of unbreakable quads
  ;; wraps = list of (list of quads)
  (let loop ([wraps null][current-wrap null][current-partial null][current-dist start-signal][qs qs])
    (match qs
      [(== empty)
       (when debug (report 'all-quads-wrapped))
       ;; combine the segments into a flat list, and drop any trailing breaks
       ;; (on the idea that breaks should separate things, and there's nothing left to separate)
       ;; wraps alternate with breaks
       (debug-report wraps)
       ;; use false as signal to indicate the end
       (define last-wrap (append-to-wrap #false (append-to-wrap current-partial current-wrap)))
       (for/list ([wrap (in-list (cons last-wrap wraps))])
         (match wrap
           [(list (? nonprinting-at-end?)) wrap] ; matches break signals
           ;; pieces will have been accumulated in reverse order
           ;; thus beginning of list represents the end of the wrap
           [(list (? (conjoin soft-break? nonprinting-at-end?)) ... rest ...)
            (debug-report (finish-wrap-proc (reverse rest)))
            (finish-wrap-proc (reverse rest))]))]
      [(cons q other-qs)
       (debug-report q 'next-q)
       (define at-start? (eq? current-dist start-signal))
       (cond
         [at-start?
          (cond
            [(and (soft-break? q) (nonprinting-at-start? q))
             (debug-report q 'skipping-soft-break-at-beginning)
             ;; skip it
             (loop wraps
                   current-wrap
                   current-partial
                   current-dist
                   other-qs)]
            [else ; printing quad
             (debug-report 'hard-quad-at-start)
             (loop wraps
                   current-wrap
                   (cons q current-partial)
                   (distance q)
                   other-qs)])]
         [else
          (define dist (if (and (quad? q) (printable? q)) (distance q) 0))
          (debug-report current-dist)
          (debug-report dist)
          (define would-overflow? (> (+ dist current-dist) target-size))
          (cond
            [would-overflow?
             (cond
               [(and (soft-break? q) (nonprinting-at-end? q))
                (debug-report 'would-overflow-soft-nonprinting)
                ;; a break is inevitable but we want to wait to finish the wrap until we see a hard quad
                ;; but we can move the current-partial into the current-wrap
                (loop wraps
                      (append-to-wrap (cons q current-partial) current-wrap)
                      null
                      (+ dist current-dist)
                      other-qs)]
               [else
                (debug-report 'would-overflow-hard)
                (debug-report (empty? current-wrap))
                ;; finish the wrap & reset the line without consuming a quad
                (if (empty? current-wrap) ; means we have not captured a soft break
                    (loop (list* (list break-val) current-partial wraps)
                          current-wrap ; which is empty
                          null
                          start-signal
                          qs)
                    (loop (list* (list break-val) current-wrap wraps)
                          null
                          current-partial
                          (apply + (map distance current-partial))
                          qs))])]
            [else
             (cond
               [(soft-break? q) ; printing soft break, like a hyphen
                (debug-report 'would-not-overflow-soft)
                ;; a soft break that fits, so move it on top of the current-wrap with the current-partial
                (cond
                  [else
                   (loop wraps
                         (append-to-wrap (cons q current-partial) current-wrap)
                         null
                         (+ dist current-dist)
                         other-qs)])]
               [else
                (debug-report 'would-not-overflow)
                ;; add to partial
                (loop wraps
                      current-wrap
                      (cons q current-partial)
                      (+ dist current-dist)
                      other-qs)])])

          ])])))


(define x (q (list 'size (pt 1 1)) #\x))
(define zwx (q (list 'size (pt 0 0)) #\z))
(define hyph (q (list 'size (pt 1 1)) #\-))
(define shy (q (list 'size (pt 1 1) 'printable? (λ (sig)
                                                  (case sig
                                                    [(end) #t]
                                                    [else #f]))) #\-))
(define a (q (list 'size (pt 1 1)) #\a))
(define b (q (list 'size (pt 1 1)) #\b))
(define c (q (list 'size (pt 1 1)) #\c))
(define d (q (list 'size (pt 1 1)) #\d))
(define sp (q (list 'size (pt 1 1) 'printable? (λ (sig)
                                                 (case sig
                                                   [(start end) #f]
                                                   [else #t]))) #\space))
(define br (q (list 'size (pt 0 0) 'printable? #f) #\newline))
(define soft-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space #\-)))))

(define (linewrap xs size [debug #f])
  (break xs size debug
         #:break-val 'lb
         #:hard-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
         #:soft-break-proc soft-break?))



(require rackunit)

(module+ test
  (test-case
   "chars"
   (check-equal? (linewrap (list) 1) null)  
   (check-equal? (linewrap (list a) 1) (list a))
   (check-equal? (linewrap (list a b) 1) (list a 'lb b))
   (check-equal? (linewrap (list a b c) 1) (list a 'lb b 'lb c))
   (check-equal? (linewrap (list a b c) 2) (list a b 'lb c))
   (check-equal? (linewrap (list x x x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 3) (list x x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 1) (list x 'lb x 'lb x 'lb x 'lb x))
   (check-equal? (linewrap (list x x x x x) 10) (list x x x x x))))

(module+ test
  (test-case
   "chars and spaces"
   (check-equal? (linewrap (list a sp b) 1) (list a 'lb b))
   (check-equal? (linewrap (list a b sp c) 2) (list a b 'lb c))
   (check-equal? (linewrap (list a sp b) 3) (list a sp b))
   (check-equal? (linewrap (list a sp b c) 3) (list a 'lb b c))))
  
(module+ test
  (test-case
   "leading & trailing spaces"
   (check-equal? (linewrap (list sp x) 2) (list x))
   (check-equal? (linewrap (list x sp) 2) (list x))
   (check-equal? (linewrap (list sp x sp) 2) (list x))
   (check-equal? (linewrap (list sp sp x sp sp) 2) (list x))
   (check-equal? (linewrap (list sp sp x sp sp x sp) 1) (list x 'lb x))))

(module+ test
  (test-case
   "hard hyphens"
   (check-equal? (linewrap (list hyph) 1) (list hyph))
   (check-equal? (linewrap (list hyph hyph) 1) (list hyph 'lb hyph))
   (check-equal? (linewrap (list hyph hyph) 2) (list hyph hyph))
   (check-equal? (linewrap (list hyph hyph hyph) 2) (list hyph hyph 'lb hyph))
   (check-equal? (linewrap (list x hyph) 1) (list x 'lb hyph))
   (check-equal? (linewrap (list a b hyph c d) 1) (list a 'lb b 'lb hyph 'lb c 'lb d))
   (check-equal? (linewrap (list a b hyph c d) 2) (list a b 'lb hyph c 'lb d))
   (check-equal? (linewrap (list a b hyph c d) 3) (list a b hyph 'lb c d))
   (check-equal? (linewrap (list x x hyph x x) 4) (list x x hyph 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 5) (list x x hyph x x))))

(module+ test
  (test-case
   "soft hyphens"
   (check-equal? (linewrap (list shy) 1) (list))
   (check-equal? (linewrap (list shy shy) 2) (list))
   (check-equal? (linewrap (list shy shy shy) 2) (list))
   (check-equal? (linewrap (list x shy) 1) (list x))
   (check-equal? (linewrap (list x shy shy shy shy) 1) (list x))
   ;; todo: degenerate cases
   ;(check-equal? (linewrap (list x x shy x x) 1) (list x 'lb x 'lb x 'lb x))
   ;(check-equal? (linewrap (list x x shy x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x shy x x) 3) (list x x shy 'lb x x))
   (check-equal? (linewrap (list x x shy x x) 4) (list x x x x))
   (check-equal? (linewrap (list x x shy x x) 5) (list x x x x))
   (check-equal? (linewrap (list x x shy x sp x) 4) (list x x x 'lb x))
   ))

(module+ test
    (test-case
     "zero width nonbreakers"
     (check-equal? (linewrap (list sp zwx) 2) (list zwx))
     (check-equal? (linewrap (list zwx sp) 2) (list zwx))
     (check-equal? (linewrap (list sp zwx sp) 2) (list zwx))
     (check-equal? (linewrap (list sp sp zwx sp sp) 2) (list zwx))
     (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) (list zwx sp sp zwx))))

(module+ test
    (test-case
     "hard breaks"
     (check-equal? (linewrap (list br) 2) (list)) ;; only insert a break if it's between things
     (check-equal? (linewrap (list a br b) 2) (list a 'lb b))
     (check-equal? (linewrap (list a b br) 2) (list a b))
     (check-equal? (linewrap (list a b br br) 2) (list a b))
     (check-equal? (linewrap (list x br x x) 3) (list x 'lb x x))
     (check-equal? (linewrap (list x x br x) 3) (list x x 'lb x))
     (check-equal? (linewrap (list x x x x) 3) (list x x x 'lb x))
     (check-equal? (linewrap (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
     (check-equal? (linewrap (list x x x sp x x) 3) (list x x x 'lb x x))))

(module+ test
    (test-case
     "hard breaks and spurious spaces"
     (check-equal? (linewrap (list a sp sp sp br b) 2) (list a 'lb b))
     (check-equal? (linewrap (list x sp br sp sp x x sp) 3) (list x 'lb x x))
     (check-equal? (linewrap (list sp sp x x sp sp br sp sp sp x) 3) (list x x 'lb x))
     (check-equal? (linewrap (list a sp b sp sp br sp c) 3) (list a sp b 'lb c))
     (check-equal? (linewrap (list x x x x) 3) (list x x x 'lb x))
     (check-equal? (linewrap (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
     (check-equal? (linewrap (list x x x sp x x) 3) (list x x x 'lb x x))))

(define (visual-wrap str int [debug #f])
  (apply string (for/list ([b (in-list (linewrap (for/list ([atom (atomize str)])
                                                   ($quad (hash-set (attrs atom) 'size '(1 1))
                                                          (elems atom))) int debug))])
                  (cond
                    [(quad? b) (car (elems b))]
                    [else #\|]))))

(module+ test
    (test-case
     "visual breaks"
     (check-equal? (visual-wrap "My dog has fleas" 1) "M|y|d|o|g|h|a|s|f|l|e|a|s")
     (check-equal? (visual-wrap "My dog has fleas" 2) "My|do|g|ha|s|fl|ea|s")
     (check-equal? (visual-wrap "My dog has fleas" 3) "My|dog|has|fle|as")
     (check-equal? (visual-wrap "My dog has fleas" 4) "My|dog|has|flea|s")
     (check-equal? (visual-wrap "My dog has fleas" 5) "My|dog|has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 6) "My dog|has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 7) "My dog|has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 8) "My dog|has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 9) "My dog|has fleas")
     (check-equal? (visual-wrap "My dog has fleas" 10) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 11) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 12) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 13) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 14) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 15) "My dog has|fleas")
     (check-equal? (visual-wrap "My dog has fleas" 16) "My dog has fleas")))


(define (pagewrap xs size [debug #f])
  (break xs size debug
         #:break-val 'pb
         #:break-before? #t
         #:hard-break-proc (λ (x) (and (quad? x) (memv (car (elems x)) '(#\page))))
         #:soft-break-proc (λ (x) (eq? x 'lb))))
(define pbr (q '(size #f) #\page))

(module+ test
    (test-case
     "soft page breaks"
     (check-equal? (pagewrap null 2) '(pb))
     (check-equal? (pagewrap (list x) 2) (list 'pb x))
     (check-equal? (pagewrap (list x x) 2) (list 'pb x x))
     (check-equal? (pagewrap (list x x x) 1) (list 'pb x 'pb x 'pb x))
     (check-equal? (pagewrap (list x x x) 2) (list 'pb x x 'pb x))
     (check-equal? (pagewrap (list x x x) 3) (list 'pb x x x))
     (check-equal? (pagewrap (list x x x) 4) (list 'pb x x x))
     (check-equal? (pagewrap (list x 'lb x x) 2) (list 'pb x 'pb x x))))

(module+ test
    (test-case
     "hard page breaks"
     (check-equal? (pagewrap (list x pbr x x) 2) (list 'pb x 'pb x x))
     (check-equal? (pagewrap (list x pbr x x) 1) (list 'pb x 'pb x 'pb x))
     (check-equal? (pagewrap (list x pbr pbr x x) 1) (list 'pb x 'pb 'pb x 'pb x))
     (check-equal? (pagewrap (list x pbr pbr x x) 2) (list 'pb x 'pb 'pb x x))
     (check-equal? (pagewrap (list 'lb x 'lb 'lb pbr 'lb x x 'lb) 2) (list 'pb x 'pb x x))))

(module+ test
    (test-case
     "composed line breaks and page breaks"
     (check-equal? (pagewrap (linewrap null 1) 2) '(pb) )
     (check-equal? (pagewrap (linewrap (list x) 1) 2) (list 'pb x))
     (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list 'pb x 'lb x 'pb x))
     (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list 'pb x x 'pb x))
     (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list 'pb x 'pb x 'pb x))))

(struct $slug $quad () #:transparent)
(define (slug . xs) ($slug #f xs))
(define (linewrap2 xs size [debug #f])
  (break xs size debug
         #:break-val 'lb
         #:hard-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
         #:soft-break-proc soft-break?
         #:finish-wrap-proc (λ (pcs) (list ($slug #f pcs)))))

(module+ test
    (test-case
     "hard breaks and spurious spaces with slugs"
     (check-equal? (linewrap2 (list a sp sp sp br b) 2) (list (slug a) 'lb (slug b)))
     (check-equal? (linewrap2 (list x sp br sp sp x x sp) 3) (list (slug x) 'lb (slug x x)))
     (check-equal? (linewrap2 (list sp sp x x sp sp br sp sp sp x) 3) (list (slug x x) 'lb (slug x)))
     (check-equal? (linewrap2 (list a sp b sp sp br sp c) 3) (list (slug a sp b) 'lb (slug c)))
     (check-equal? (linewrap2 (list x x x x) 3) (list (slug x x x) 'lb (slug x)))
     (check-equal? (linewrap2 (list x x x sp x x) 2) (list (slug x x) 'lb (slug x) 'lb (slug x x)))
     (check-equal? (linewrap2 (list x x x sp x x) 3) (list (slug x x x) 'lb (slug x x)))))
