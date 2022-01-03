#lang debug racket/base
(require (for-syntax racket/base)
         racket/match
         racket/path
         racket/dict
         pollen/tag
         racket/system
         racket/class
         racket/string
         syntax/strip-context
         scribble/reader
         quadwriter/core
         txexpr
         quad/log
         "log.rkt")
(provide (all-defined-out))

(define q (default-tag-function 'q))


(define ((make-read-syntax expander-mod pt-proc) path-string p)
  ;; peel off any lines of format #:keyword val (bounded by newline)
  ;; and turn them into qexpr attrs
  (define kw-attrs
    (let loop ([kw-val-pairs null])
      (cond
        [(regexp-try-match #px"^\\s+" p) (loop kw-val-pairs)]
        [(equal? (peek-bytes 2 0 p) #"#:") ; discovered a keyword
         (parameterize ([current-input-port (open-input-string (read-line p))])
           (match (list (read) (read))
             [(list (? keyword? kw) val) #:when (not (eof-object? val))
                                         (loop (cons (cons kw val) kw-val-pairs))]
             [_ (loop kw-val-pairs)]))]
        ;; reverse in case of multiple values with same keyword, latest takes precedence (by becoming first)
        [else (reverse (for/list ([(kw val) (in-dict kw-val-pairs)])
                                 (list (string->symbol (string-trim (keyword->string kw) "#:"))
                                       (format "~a" val))))])))
  (strip-context
   (with-syntax ([PATH-STRING path-string]
                 [((ATTR-NAME ATTR-VAL) ...) kw-attrs]
                 [PT (pt-proc path-string p)]
                 [EXPANDER-MOD expander-mod])
     #'(module _ EXPANDER-MOD
         PATH-STRING
         ((ATTR-NAME ATTR-VAL) ...)
         . PT))))

(define-syntax-rule (make-module-begin DOC-PROC)
  (begin
    (provide (rename-out [new-module-begin #%module-begin]))
    (define-syntax (new-module-begin stx)
      (syntax-case stx ()
        [(_ PATH-STRING ATTRS . EXPRS)
         (with-syntax ([DOC (datum->syntax #'PATH-STRING 'doc)]
                       [VIEW-OUTPUT (datum->syntax #'PATH-STRING 'view-output)])
           #'(#%module-begin
              (provide DOC VIEW-OUTPUT)
              (define attrs (let ([h (make-hasheq)])
                              (for ([kvlist (in-list 'ATTRS)])
                                   (apply hash-set! h kvlist))
                              h))
              (define pdf-path (hash-ref! attrs 'output-path (λ () (path-string->pdf-path-string 'PATH-STRING))))
              (define DOC `(q ,(for/list ([(k v) (in-hash attrs)])
                                         (list k v))
                              ,(DOC-PROC (list . EXPRS))))

              (define (VIEW-OUTPUT)
                (when (file-exists? pdf-path)
                  (define open-string
                    (case (system-type 'os)
                      [(macosx) "open '~a'"]
                      [(windows) "start '~a'"]
                      [(unix) "xdg-open '~a' &> /dev/null"]
                      [else (error "Unknown platform. Don't know how to view PDF.")]))
                  (void (system (format open-string pdf-path)))))

              (define (make-pdf [the-pdf-path #false])
                (with-logging-to-port
                 (current-output-port)
                 (λ () (with-logging-to-port
                        (current-output-port)
                        (λ () (render-pdf DOC the-pdf-path))
                        #:logger quadwriter-logger
                        'debug))
                 #:logger quad-logger
                 'debug))

              (module+ pdf
                (define pdf (make-pdf pdf-path))
                (provide pdf))
              
              (module+ main
                (make-pdf pdf-path))))]))))

(define (path-string->pdf-path-string path-string)
  (path->string
   (match (format "~a" path-string)
     ;; weird test but sometimes DrRacket calls the unsaved file
     ;; 'unsaved-editor and sometimes "unsaved editor"
     [(regexp #rx"unsaved.editor")
      (build-path (find-system-path 'desk-dir) "untitled.pdf")]
     [_ (define path-without-ext (path-replace-extension path-string #""))
        ;; do the right thing with pollen-style double extension like "foo.pdf.pm"
        (if (path-has-extension? path-without-ext #".pdf")
            path-without-ext
            (path-add-extension path-without-ext #".pdf"))])))

(define quad-at-reader (make-at-reader
                        #:syntax? #t 
                        #:inside? #t
                        #:command-char #\◊))

(define (xexpr->parse-tree x)
  ;; an ordinary txexpr can't serve as a parse tree because of the attrs list fails when passed to #%app.
  ;; so stick an `attr-list` identifier on it which can hook into the expander.
  ;; sort of SXML-ish.
  (let loop ([x x])
    (match x
      [(txexpr tag attrs elems) (list* tag (cons 'attr-list attrs) (map loop elems))]
      [(? list? xs) (map loop xs)]
      [_ x])))

(define (get-info-texty in mod line col pos)
  ;; DrRacket caches source file information per session,
  ;; so we can do the same to avoid multiple searches for the command char.
  (define command-char-cache (make-hash))
  (define my-command-char #\◊)
  (λ (key default)
    (case key
      [(color-lexer)
       (match (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #false))
         [(? procedure? make-lexer) (make-lexer #:command-char my-command-char)]
         [_ default])]
      [(drracket:toolbar-buttons)
       (match (dynamic-require 'pollen/private/drracket-buttons 'make-drracket-buttons (λ () #false))
         [(? procedure? make-buttons) (make-buttons my-command-char)])]
      [(drracket:indentation)
       (λ (text pos)
         (define line-idx (send text position-line pos))
         (define line-start-pos (send text line-start-position line-idx))
         (define line-end-pos (send text line-end-position line-idx))
         (define first-vis-pos
           (or
            (for/first ([pos (in-range line-start-pos line-end-pos)]
                        #:unless (char-blank? (send text get-character pos)))
                       pos)
            line-start-pos))
         (- first-vis-pos line-start-pos))]      
      [else default])))

(define (get-info-rackety in mod line col pos) (λ (key default) default))