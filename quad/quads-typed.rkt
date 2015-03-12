#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax racket/string))
(require/typed racket/list [empty? (All (A) ((Listof A) -> Boolean))]
               [last ((Listof Any) . -> . Any)]
               [flatten ((Listof Any) . -> . (Listof Any))])
(require/typed sugar/list [trimf (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))]
               [filter-split (All (A) ((Listof A) (A . -> . Boolean) -> (Listof (Listof A))))])
(require/typed racket/string [string-append* ((Listof String) . -> . String)])
(require/typed sugar/string [ends-with? (String String . -> . Boolean)])
(require sugar/debug)
(provide (all-defined-out))


(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

(define-syntax (define/typed+provide stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(begin
         (provide proc-name)
         (define/typed proc-name type-expr
           (λ(arg ... . rest-arg) body ...)))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (provide proc-name)
         (begin
         (: proc-name type-expr)
         (define proc-name body ...)))]))


(define-syntax-rule (even-members xs)
  (for/list : (Listof Any) ([(x i) (in-indexed xs)] #:when (even? i))
    x))

(: hashable-list? (Any . -> . Boolean))
(define (hashable-list? x) (and (list? x) (even? (length x)) (andmap symbol? (even-members x))))

(define-type QuadName Symbol)
(define-predicate QuadName? QuadName)

(define-type QuadAttrKey Symbol)
(define-predicate QuadAttrKey? QuadAttrKey)
(define-type QuadAttrValue Any)
(define-predicate QuadAttrValue? QuadAttrValue)
(define-type QuadAttrs (HashTable QuadAttrKey QuadAttrValue))
(define-type HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(define-predicate HashableList? HashableList)


(: quad-attrs? (Any . -> . Boolean))
(define (quad-attrs? x)
  (and (hash? x) (andmap QuadAttrKey? (hash-keys x))))

(define-type QuadListItem (U Quad String))
(define-type QuadList (Listof QuadListItem))
(define-type (Treeof A) (Rec as (U A (Listof as))))

(struct quad ([name : QuadName] [attrs : QuadAttrs] [list : QuadList]) #:transparent
  #:property prop:sequence (λ(q) (quad-list q)))

(define-type Quad quad)
(define-predicate Quad? Quad)


(define quad-attr-ref
  (case-lambda
    [([q : Quad] [key : QuadAttrKey]) 
     (hash-ref (quad-attrs q) key)]
    [([q : Quad] [key : QuadAttrKey] [default : QuadAttrValue]) 
     (hash-ref (quad-attrs q) key (λ() default))]))

(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))

(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))
(define-type QuadAttrPair (Pairof QuadAttrKey QuadAttrValue))


(: quad-ends-with? (Quad String . -> . Boolean))
(define (quad-ends-with? q str)
  (cond
    [(not (empty? (quad-list q)))
     (define last-item (last (quad-list q)))
     (cond
       [(string? last-item) (ends-with? last-item str)]
       [(quad? last-item) (quad-ends-with? last-item str)]
       [else #f])]
    [else #f]))

(: quad-append (Quad QuadListItem . -> . Quad))
(define (quad-append q new-item)
  (quad (quad-name q) (quad-attrs q) (append (quad-list q) (list new-item))))


(: quad->string (Quad . -> . String))
(define (quad->string x)
  (let loop : String ([x : (U Quad String) x])
    (cond
      [(quad? x) (string-append* ((inst map String QuadListItem) loop (quad-list x)))]
      [(string? x) x]
      [else ""])))

(define/typed+provide (gather-common-attrs qs)
  ((Listof Quad) . -> . (U False HashableList))
  (: check-cap (QuadAttrPair . -> . Boolean))
  (define (check-cap cap)
    (equal? (quad-attr-ref (car qs) (car cap) attr-missing) (cdr cap)))
  (let loop 
    ([qs qs]
     [common-attr-pairs : (Listof QuadAttrPair) (if (quad-attrs (car qs))
                                                    
                                                    (for/list ([kv-pair (in-hash-pairs (quad-attrs (car qs)))] 
                                                               #:unless (member (car kv-pair) cannot-be-common-attrs))  
                                                      kv-pair)
                                                    null)])
    (cond
      [(null? common-attr-pairs) #f]
      [(null? qs) (cast (flatten common-attr-pairs) HashableList)] ;; flatten + cast needed because this output gets used by quadattrs
      ;; todo: reconsider type interface between output of this function and input to quadattrs
      [else (loop (cdr qs) (filter check-cap common-attr-pairs))])))



(: quadattrs ((Listof Any) . -> . QuadAttrs))
(define (quadattrs xs)
  (let-values ([(ks vs even?) (for/fold 
                               ([ks : (Listof Any) null][vs : (Listof Any) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if even?
                                    (values (cons x ks) vs #f)
                                    (values ks (cons x vs) #t)))]) 
    (when (not even?) (error 'quadattrs "odd number of elements in ~a" xs))
    (cast (for/hash ([k (in-list ks)][v (in-list vs)])
            (values k v)) QuadAttrs)))



(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([id? (format-id #'id "~a?" #'id)]
                   [quads->id (format-id #'id "quads->~a" #'id)])
       #'(begin
           ;; quad converter
           (: quads->id ((Listof Quad) . -> . Quad))
           (define (quads->id qs)
             (apply id (gather-common-attrs qs) qs))
           
           (: id (case-> 
                  (-> Quad)
                  ((U QuadAttrs (Listof Any) False) (U String Quad) * . -> . Quad)))
           (define (id [attrs #f] . xs)
             (quad 'id (cond 
                         [(quad-attrs? attrs) (cast attrs QuadAttrs)]
                         [(list? attrs)
                          (if (hashable-list? attrs)
                              (quadattrs attrs)
                              (error 'id "got non-hashable list ~a" attrs))]
                         [else (quadattrs '())]) (cast xs QuadList)))
           
           (: id? (Any . -> . Boolean))
           (define (id? x)
             (and (quad? x) (equal? (quad-name x) 'id)))
           
           ))]))

(: whitespace? ((Any) (Boolean) . ->* . Boolean))
(define (whitespace? x [nbsp? #f])
  ;((any/c)(boolean?) . ->* . coerce/boolean?)
  (cond
    [(quad? x) (whitespace? (quad-list x) nbsp?)]
    [(string? x) (or (and (regexp-match #px"\\p{Zs}" x) ; Zs = unicode whitespace category
                          (or nbsp? (not (regexp-match #px"\u00a0" x)))))] ; 00a0: nbsp
    [(list? x) (and (not (empty? x)) (andmap (λ(x) (whitespace? x nbsp?)) x))] ; andmap returns #t for empty lists
    [else #f]))

(define (whitespace/nbsp? x)
  (whitespace? x #t))

(define-syntax (define-break-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([split-on-id-breaks (format-id #'id "split-on-~a-breaks" #'id)]
                   [id-break (format-id #'id "~a-break" #'id)]
                   [id-break? (format-id #'id "~a-break?" #'id)]
                   [multi-id (format-id #'id "multi~a" #'id)]
                   [multi-id? (format-id #'id "multi~a?" #'id)]
                   [quads->multi-id (format-id #'id "quads->multi~a" #'id)])
       #'(begin
           (define-quad-type id)
           (define-quad-type id-break)
           (define-quad-type multi-id)
           ;; breaker
           (: split-on-id-breaks ((Listof Quad) . -> . (Listof (Listof Quad))))
           (define (split-on-id-breaks xs)
             ;; omit leading & trailing whitespace, because they're superfluous next to a break
             (map (λ([xs : (Listof Quad)]) (trimf xs whitespace?)) (filter-split xs id-break?)))))]))

(define quad= equal?)

(: quad-has-attr? (Quad QuadAttrKey . -> . Boolean))
(define (quad-has-attr? q key)
  (hash-has-key? (quad-attrs q) key))

(define-quad-type spacer)
(define-quad-type kern)
(define-quad-type optical-kern)
(define-quad-type flag)
(define-quad-type doc)
(define-quad-type input)
(define-quad-type piece)
(define-quad-type run)
(define-quad-type box)


(define-break-type word)
(: word-string (Quad . -> . String))
(define (word-string c) 
  (cast ((inst car QuadListItem Any) (quad-list c)) String))
(define-break-type page)
(define-break-type column)
(define-break-type block)
(define-break-type line)

