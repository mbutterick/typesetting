#lang debug racket
(require xenomorph
         "tables.rkt"
         "struct.rkt"
         (for-syntax "tables.rkt"))
(provide (all-defined-out))

(define-syntax (define-table-getters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(TABLE-TAG ...) (hash-keys table-codecs)])
       (with-syntax ([(GETTER-ID ...) (map (λ (tag) (datum->syntax stx (string->symbol (format "get-~a-table" (syntax->datum tag)))))
                                           (syntax->list #'(TABLE-TAG ...)))]
                     [(HAS-ID? ...) (map (λ (tag) (datum->syntax stx (string->symbol (format "has-~a-table?" (syntax->datum tag)))))
                                         (syntax->list #'(TABLE-TAG ...)))])
         #'(begin
             (define (GETTER-ID this) (get-table this 'TABLE-TAG)) ...
             (define (HAS-ID? this) (has-table? this 'TABLE-TAG)) ...)))]))

(define (has-table? this tag)
  #;((or/c bytes? symbol?) . ->m . boolean?)
  (define directory (force (ttf-font-directory this)))
  (hash-has-key? (hash-ref directory 'tables) (match tag
                                                [(? bytes?) (string->symbol (bytes->string/latin-1 tag))]
                                                [_ tag])))

(define (get-table this table-tag)
  (unless (has-table? this table-tag)
    (raise-argument-error 'get-table "table that exists in font" table-tag))
  (hash-ref! (ttf-font-decoded-tables this) table-tag (λ () (decode-table this table-tag))))

(define-table-getters)

(define (get-table-stream this tag)
  (define directory (force (ttf-font-directory this)))
  (define table (hash-ref (hash-ref directory 'tables) tag))
  (and table (pos (ttf-font-port this) (hash-ref table 'offset)) (ttf-font-port this)))
  
(define (decode-table this table-tag)
  (unless (hash-has-key? table-codecs table-tag)
    (raise-argument-error 'decode-table "decodable table" table-tag))

  (define last-pos (pos (ttf-font-port this)))
  (define stream (get-table-stream this table-tag))

  (define table-decoder (hash-ref table-codecs table-tag))
  (define directory (force (ttf-font-directory this)))
  (define table (hash-ref (hash-ref directory 'tables) table-tag))

  (begin0
    (decode table-decoder stream #:parent this (hash-ref table 'length))
    (pos (ttf-font-port this) last-pos)))

