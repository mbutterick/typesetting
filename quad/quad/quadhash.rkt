#lang debug racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct
         racket/format
         racket/list
         racket/string
         racket/promise
         racket/dict
         racket/match
         "param.rkt"
         "rebase.rkt")
(provide (all-defined-out))

(module+ test (require rackunit))

(define-syntax (define-quad-attribute stx)
  (syntax-case stx ()
    [(_ ATTR)
     (with-syntax ([QUAD-ATTR (format-id #'ATTR "quad-~a" #'ATTR)]
                   [QUAD-ATTR-SET! (format-id #'ATTR "quad-~a-set!" #'ATTR)])
       #'(begin
           (define (QUAD-ATTR q) (hash-ref q 'ATTR #false))
           (define (QUAD-ATTR-SET! q val) (hash-set! q 'ATTR val) q)))]
    [(_ ATTR ...)
     #'(begin
         (define-quad-attribute ATTR) ...)]))

(define-quad-attribute size)

(define (size q)
  (match (quad-size q)
    [(? procedure? proc) proc (proc q)]
    [(? promise? prom) (force prom)]
    [val val]))

(define-quad-attribute printable)

(define (printable? q [signal #f])
  (match (quad-printable q)
    [(? procedure? proc) (proc q signal)]
    [val val]))

(define-quad-attribute draw-start draw draw-end)

(define (draw q [surface (current-output-port)])
  ((or (quad-draw-start q) void) q surface)
  ((or (quad-draw q) void) q surface)
  ((or (quad-draw-end q) void) q surface))

(define-quad-attribute elems shift from-parent from to shift-elems origin)

(define quad=? equal?)

;; keep this param here so you don't have to import quad/param to get it
(define verbose-quad-printing? (make-parameter #f))

(define quad? hash?)

#;(struct quad (
              ;; WARNING
              ;; atomize procedure depends on attrs & elems
              ;; being first two fields of struct.
              attrs ; key-value pairs, arbitrary
              elems ; subquads or text
              ;; size is a two-dim pt
              size ; outer size of quad for layout (though not necessarily the bounding box for drawing)
              ;; from-parent, from, to are phrased in terms of cardinal position
              from-parent ; alignment point on parent. if not #f, supersedes `from`
              ;; (this way, `from` doens't change, so a quad can "remember" its default `from` attachment point)
              from ; alignment point on ref quad
              to ; alignment point on this quad that is matched to `from` on previous quad
              ;; shift-elements, shift are two-dim pts
              ;; shift-elements = Similar to `relative` CSS positioning
              ;; moves origin for elements . Does NOT change layout position of parent.
              shift-elems
              ;; shift = shift between previous out point and current in point.
              ;; DOES change the layout position.
              shift
              ;; reference point (in absolute coordinates)
              ;; for all subsequent drawing ops in the quad. Calculated, not set directly
              origin 
              printable ; whether the quad will print
              draw-start ; func called at the beginning of every draw event (for setup ops)
              draw ; func called in the middle of every daw event
              draw-end ; func called at the end of every draw event (for teardown ops)
              name ; for anchor resolution
              tag) ; from q-expr, maybe
  #:mutable
  #:transparent
  #:property prop:custom-write
  (λ (q p w?) (display
               (format "<~a-~a~a~a>"
                       (quad-tag q)
                       (object-name q)
                       (if (verbose-quad-printing?)
                           (string-join (map ~v (flatten (hash->list (quad-attrs q))))
                                        " " #:before-first "(" #:after-last ")")
                           "")
                       (match (quad-elems q)
                         [(? pair?) (string-join (map ~v (quad-elems q)) " " #:before-first " ")]
                         [_ ""])) p))
  #:methods gen:equal+hash
  [(define equal-proc quad=?)
   (define (hash-proc h recur) (equal-hash-code h))
   (define (hash2-proc h recur) (equal-secondary-hash-code h))])

#;(struct quad-attr (key default-val) #:transparent)

#;(define (make-quad-attr key [default-val #f])
    (quad-attr key default-val))

(define quad-ref hash-ref)

(define quad-set! hash-set!)

(define-syntax (quad-copy stx)
  (syntax-case stx ()
    [(_ QUAD-TYPE ID [K V] ...)
     (if (free-identifier=? #'quad #'QUAD-TYPE)
         #'(struct-copy QUAD-TYPE ID
                        [K V] ...)
         #'(struct-copy QUAD-TYPE ID
                        [K #:parent quad V] ...))]))

#;(define-syntax (quad-update! stx)
  (syntax-case stx ()
    [(_ ID [K V] ...)
     (with-syntax ([(K-SETTER ...) (for/list ([kstx (in-list (syntax->list #'(K ...)))])
                                             (format-id kstx "set-quad-~a!" kstx))])
       #'(let ([q ID])
           (K-SETTER q V) ...
           q))]))

(define (default-printable q [sig #f]) #t)

(define (default-draw q surface)
  (for-each (λ (qi) (draw qi surface)) (quad-elems q)))

;; why 'nw and 'ne as defaults for in and out points:
;; if size is '(0 0), 'nw and 'ne are the same point,
;; and everything piles up at the origin
;; if size is otherwise, the items don't pile up (but rather lay out in a row)

#;(define (make-quad-constructor type)
  (make-keyword-procedure (λ (kws kw-args . rest)
                            (keyword-apply make-quad #:type type kws kw-args rest))))

(define (derive-quad-constructor q)
  (define-values (x-structure-type _) (struct-info q))
  (struct-type-make-constructor x-structure-type))

  
(define q make-hash)

(define only-prints-in-middle (λ (q sig) (not (memq sig '(start end)))))

(define/match (from-parent qs [where #f])
  ;; doesn't change any positioning. doesn't depend on state. can happen anytime.
  ;; can be repeated without damage.
  [((? null?) _) null]
  [((cons q rest) where)
   (quad-set! q [from-parent (or where (quad-from q))])
   (cons q rest)])

(module+ test
  (require racket/port)
  (define q1 (q '((elems #\H #\e #\l #\o))))
  (define q2 (q '((elems #\H #\e #\l #\o))))
  (define q3 (q '((elems #\H #\e #\l))))
  (check-true (equal? q1 q1))
  (check-true (equal? q1 q2))
  (check-false (equal? q1 q3))
  (define q4 (quad-draw-set! (hash-copy q1) (λ (q surface) (display "foo" surface))))
  (check-equal? (with-output-to-string (λ () (draw q4))) "foo"))