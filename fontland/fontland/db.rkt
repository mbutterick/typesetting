#lang debug racket
(require sugar/debug sugar/cache racket/class racket/match
         db racket/logging racket/draw openssl/sha1 racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path db-file "fontland.sqlite")
(define current-query-debug (make-parameter #f))
(define current-dbc (make-parameter (sqlite3-connect #:database db-file #:mode 'create)))

(define-logger db)

(define (log-query q) (when (current-query-debug) (log-db-info q)))

(define-syntax-rule (query-exec-logging q arg ...)
  (begin (log-query q) (query-exec (current-dbc) q arg ...)))

(define-syntax-rule (query-rows-logging q arg ...)
  (begin (log-query q) (query-rows (current-dbc) q arg ...)))

(define (add-record! rec)
  (define recstring (format "(~a, '~a')" (car rec) (bytes->hex-string (cdr rec))))
  (query-exec-logging (format "insert or replace into layouts (crc, layout) values ~a" recstring)))

(define/caching (get-layout-from-db which)
  (match (query-rows-logging (format "select layout from layouts where crc==~a" which))
    [(list (vector val)) (hex-string->bytes val)]
    [_ #false]))

(define (init-db)
  (query-exec-logging "create table if not exists layouts (crc INTEGER UNIQUE, layout TEXT)")
  (query-exec-logging "create index if not exists layout_crcs ON layouts (crc);
"))

(module+ main
  (init-db)
  (add-record! (cons 42 #"01234"))
  (get-layout-from-db 42))