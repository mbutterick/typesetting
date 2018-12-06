#lang debug racket
(require sugar/unstable/js
         (only-in xenomorph pos decode)
         "tables.rkt"
         (for-syntax "tables.rkt"))
(provide (all-defined-out))

(define-syntax (define-table-getters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(TABLE-TAG ...) (hash-keys table-codecs)])
       (with-syntax ([(GETTER-ID ...) (map (λ (tag) (datum->syntax stx (string->symbol (format "get-~a-table" (syntax->datum tag)))))
                                           (syntax->list #'(TABLE-TAG ...)))])
       #'(begin
           (define (GETTER-ID this) (get-table this 'TABLE-TAG)) ...)))]))

(define (has-table? this tag)
  #;((or/c bytes? symbol?) . ->m . boolean?)
  (hash-has-key? (· this directory tables) (match tag
                                             [(? bytes?) (string->symbol (bytes->string/latin-1 tag))]
                                             [_ tag])))
  


(define (get-table this table-tag)
  (unless (has-table? this table-tag)
    (raise-argument-error 'get-table "table that exists in font" table-tag))
  (hash-ref! (· this  _decoded-tables) table-tag (λ () (decode-table this table-tag))))

(define-table-getters)

(define (get-table-stream this tag)
  (define table (hash-ref (· this directory tables) tag))
  (and table (pos (· this _port) (· table offset)) (· this _port)))
  
(define  (decode-table this table-tag)
  (unless (hash-has-key? table-codecs table-tag)
    (raise-argument-error 'decode-table "decodable table" table-tag))
  (define table (hash-ref (· this directory tables) table-tag))
  ;; todo: possible to avoid copying the bytes here?
  (pos (· this _port) (· table offset))
  (define table-bytes (open-input-bytes (peek-bytes (· table length) 0 (· this _port))))
  (define table-decoder (hash-ref table-codecs table-tag))
  (decode table-decoder table-bytes #:parent this))

