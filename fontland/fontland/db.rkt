#lang debug racket
(require sugar/cache racket/match
         db racket/runtime-path)
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
  (query-exec-logging "insert or replace into layouts (crc, layout) values ($1, $2)" (car rec) (cdr rec)))

(define (get-layout-from-db which)
  (match (query-rows-logging "select layout from layouts where crc==$1" which)
    [(list (vector val)) val]
    [_ #false]))

(define (init-db)
  (query-exec-logging "create table if not exists layouts (crc INTEGER UNIQUE, layout BLOB)")
  (query-exec-logging "create index if not exists layout_crcs ON layouts (crc);
"))

(module+ main
  (init-db)
  (add-record! (cons 42 (bytes 0 1 0 2 3 4)))
  (get-layout-from-db 42))