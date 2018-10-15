#lang debug racket

(struct $csp (variables domains neighbors constraints initial curr_domains nassigns) #:transparent #:mutable)

(define/contract (make-csp variables domains neighbors constraints)
  ((listof symbol?) hash? hash? procedure? . -> . $csp?)
  ($csp
   variables
   domains
   neighbors
   constraints
   null
   #f
   0))

(define/contract (assign csp var val assignment)
  ($csp? symbol? any/c hash? . -> . void?)
  ;; Add {var: val} to assignment; Discard the old value if any.
  (hash-set! assignment var val)
  (set-$csp-nassigns! csp (add1 ($csp-nassigns csp))))

(define/contract (unassign csp var assignment)
  ($csp? symbol? hash? . -> . void?)
  ;; Remove {var: val} from assignment.
  ;; DO NOT call this if you are changing a variable to a new value;
  ;; just call assign for that.
  (hash-remove! assignment var))

(define/contract (nconflicts csp var val assignment)
  ($csp? symbol? any/c hash? . -> . number?)
  ;; Return the number of conflicts var=val has with other variables."""
  ;; Subclasses may implement this more efficiently
  (define (conflict var2)
    (and (hash-has-key? assignment var2)
         (not (($csp-constraints csp) var val var2 (hash-ref assignment var2)))))
  (for/sum ([v (hash-ref ($csp-neighbors csp) var)]
            #:when (conflict v))
    1))

(define (display csp assignment)
  (displayln "todo"))

(define/contract (support_pruning csp)
  ($csp? . -> . void?)
  ;; Make sure we can prune values from domains. (We want to pay
  ;; for this only if we use it.)
  (when (false? ($csp-curr_domains csp))
    (set-$csp-curr_domains!
     csp
     (let ([h (make-hash)])
       (for ([v ($csp-variables csp)])
         (hash-set! h v (hash-ref ($csp-domains csp) v)))
       h))))
      
(define/contract (suppose csp var value)
  ($csp? symbol? any/c . -> . (listof (cons/c symbol? any/c)))
  ;; Start accumulating inferences from assuming var=value
  (support_pruning csp)
  (define removals
    (for/list ([a (hash-ref ($csp-curr_domains csp) var)]
               #:when (not (equal? a value)))
      (cons var a)))
  (hash-set! ($csp-curr_domains csp) var (list value))
  removals)


(define/contract (prune csp var value removals)
  ($csp? symbol? any/c (or/c #f (listof (cons/c symbol? any/c))) . -> . (listof (cons/c symbol? any/c)))
  ;; Rule out var=value
  (hash-update! ($csp-curr_domains csp) var
                (λ (vals) (remove value vals)))
  (and removals
       (append removals (list (cons var value)))))

(define/contract (choices csp var)
  ($csp? symbol? . -> . (listof any/c))
  ;; Return all values for var that aren't currently ruled out.
  (hash-ref (or ($csp-curr_domains csp) ($csp-domains csp)) var))

(define/contract (infer_assignment csp)
  ($csp? . -> . hash?)
  ;; Return the partial assignment implied by the current inferences.
  (support_pruning csp)
  (let ([a (make-hash)])
    (for ([v ($csp-variables csp)]
          #:when (= 1 (length (hash-ref ($csp-curr_domains csp) v))))
      (hash-set! a v (first (hash-ref ($csp-curr_domains csp) v))))
    a))

(define/contract (restore csp removals)
  ($csp? (listof (cons/c symbol? any/c)) . -> . void?)
  ;; Undo a supposition and all inferences from it.
  (for ([removal removals])
    (match-define (cons B b) removal)
    (hash-update! ($csp-curr_domains csp) B
                  (λ (vals) (append vals (list b))))))


;; ______________________________________________________________________________
;; CSP Backtracking Search

;; Variable ordering

(define/contract (first_unassigned_variable assignment csp)
  (hash? $csp? . -> . symbol?)
  ;; The default variable order.
  (for/first ([var ($csp-variables csp)]
              #:when (not (hash-has-key? assignment var)))
    var))

;; Value ordering

(define/contract (unordered_domain_values var assignment csp)
  (symbol? hash? $csp? . -> . (listof any/c))
  ;; The default value order.
  (choices csp var))

;; Inference

(define/contract (no_inference csp var value assignment removals)
  ($csp? symbol? any/c hash? (listof (cons/c symbol? any/c)) . -> . boolean?)
  #true)

(define/contract (backtracking_search csp
                                      [select_unassigned_variable first_unassigned_variable]
                                      [order_domain_values unordered_domain_values]
                                      [inference no_inference])
  (($csp?) (procedure? procedure? procedure?) . ->* . (or/c #f hash?))
  #f)

(require rackunit)
(define vs '(wa nsw t q nt v sa))
(define ds (for/hash ([k vs])
             (values k '(red green blue))))
(define ns (for*/hash ([k vs]
                       [k2 (cdr vs)])
             (values k (list k2))))
(define csp (make-csp vs ds ns void))
(check-true ($csp? csp))
(define a (make-hash))
(assign csp 'key 42 a)
(check-equal? (hash-ref a 'key) 42)
(unassign csp 'key a)
(check-exn exn:fail? (λ () (hash-ref a 'key)))
(check-equal? 0 (nconflicts csp 'wa 'red (hasheq 'wa 42)))
(support_pruning csp)
(check-true (hash? ($csp-curr_domains csp)))

(check-equal?
 (suppose csp 'wa 'red)
 '((wa . green) (wa . blue)))
(check-equal?
 (hash-ref ($csp-curr_domains csp) 'wa) '(red))

(check-equal? (prune csp 'v 'red empty) '((v . red)))

(check-equal? (choices csp 'v) '(green blue))
(check-equal? (choices csp 'wa) '(red))
(check-equal? (infer_assignment csp)
              (make-hash '((wa . red))))
(check-equal? (suppose csp 'v 'blue) '((v . green)))
(check-equal? (infer_assignment csp)
              (make-hash '((v . blue) (wa . red))))
(restore csp '((wa . green)))
(check-equal? (infer_assignment csp)
              (make-hash '((v . blue))))

(check-equal? (first_unassigned_variable (hash) csp) 'wa)
(check-equal? (unordered_domain_values 'wa (hash) csp) '(red green))

(backtracking_search csp)