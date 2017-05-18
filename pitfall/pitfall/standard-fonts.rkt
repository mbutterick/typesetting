#lang pitfall/racket
(require racket/runtime-path (for-syntax racket/base racket/path racket/syntax))
(provide isStandardFont standard-fonts)

(define (isStandardFont name) (hash-ref standard-fonts name #f))

(define-syntax (define-afm-table stx)
  (syntax-case stx ()
    [(_ hashid dir)
     (let* ([path-strings (for/list ([p (in-directory (syntax->datum #'dir) #t)]
                                     #:when (and (file-exists? p) (path-has-extension? p #"afm") p))
                            (path->string p))]
            [id-strings (for/list ([pstr (in-list path-strings)])
                          (path->string (cadr (explode-path (path-replace-extension pstr #"")))))])
       (with-syntax ([(PATH-STR ...) path-strings]
                     [(ID-STR ...) id-strings]
                     [(ID ...) (map (λ (id-str) (format-id #'hashid "~a" id-str)) id-strings)])
         #'(begin (define-runtime-path ID PATH-STR) ...
                  (define hashid (make-hash (list (cons ID-STR (procedure-rename (λ () (file->string ID)) 'ID)) ...))))))]))

(define-afm-table standard-fonts "data")


(module+ test
  (require rackunit)
  (check-true (and (isStandardFont "Helvetica") #t))
  (check-true (and (isStandardFont "Courier") #t))
  (check-true (and (isStandardFont "ZapfDingbats") #t))
  (check-false (isStandardFont "Not A Font Name")))
