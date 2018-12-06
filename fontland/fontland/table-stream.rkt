#lang racket
(require sugar/unstable/js
         (only-in xenomorph pos decode)
         "tables.rkt"
         (for-syntax "tables.rkt"))
(provide (all-defined-out))

(define-syntax (define-table-getters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(TABLE-TAG ...) (hash-keys table-codecs)])
       #'(begin
           (define (TABLE-TAG this) (_getTable this 'TABLE-TAG)) ...))]))

(define (has-table? this tag)
  #;((or/c bytes? symbol?) . ->m . boolean?)
  (hash-has-key? (· this directory tables) (match tag
                                             [(? bytes?) (string->symbol (bytes->string/latin-1 tag))]
                                             [_ tag])))
  


(define (_getTable this table-tag)
  (unless (has-table? this table-tag)
    (raise-argument-error '_getTable "table that exists in font" table-tag))
  (hash-ref! (· this  _decoded-tables) table-tag (λ () (_decodeTable this table-tag))))

(define-table-getters)

(define (_getTableStream this tag)
  (define table (hash-ref (· this directory tables) tag))
  (and table (pos (· this _port) (· table offset)) (· this _port)))
  
(define  (_decodeTable this table-tag)
  (unless (hash-has-key? table-codecs table-tag)
    (raise-argument-error '_decodeTable "decodable table" table-tag))
  (define table (hash-ref (· this directory tables) table-tag))
  ;; todo: possible to avoid copying the bytes here?
  (pos (· this _port) (· table offset))
  (define table-bytes (open-input-bytes (peek-bytes (· table length) 0 (· this _port))))
  (define table-decoder (hash-ref table-codecs table-tag))
  (decode table-decoder table-bytes #:parent this))

