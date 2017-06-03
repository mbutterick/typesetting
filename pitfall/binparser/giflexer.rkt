#lang br
(require brag/support "gifparser.rkt")

(define the-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ["GIF" (token 'GIF-HEADER lexeme)]
   [any-char (token 'BYTE (char->integer (car (string->list lexeme))))]
   ))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (λ () (the-lexer ip)))

(define (munge ip)
  (parse-to-datum (make-tokenizer (reencode-input-port ip "latin1"))))


(module+ test
  (require pitfall/binprint)
  #;(binprint (open-input-file "test.gif") #:width 24)
  
  #;(munge (open-input-bytes #"GIF87a1234567"))
  (munge (open-input-file "test.gif"))
  )