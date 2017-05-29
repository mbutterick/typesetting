#lang pitfall/racket
(provide (all-defined-out))

(define-subclass object% (TTFFont buffer)
  (super-new)

  (define (buffer->font buffer)
    'made-ttf-font)

  (define (probe buffer)
    (and
     (member (bytes->string/latin-1 (subbytes buffer 0 4))
             (list "true" "OTTO" "\u0\u1\u0\u0"))
     'TTF-format))
  
  (and (probe buffer) (buffer->font buffer)))


;; Register font formats
(define formats (list TTFFont))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo

(define/contract (create buffer [postscriptName #f])
  ((bytes?) ((or/c string? #f)) . ->* . any/c)
  (or
   (for*/first ([format (in-list formats)]
                [font (in-value (make-object format buffer))]
                #:when font)
     (if postscriptName
         (send font getFont postscriptName)
         font))
   (error 'create "unknown font format")))


(define/contract (openSync filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . any/c)
  (define buffer (file->bytes filename))
  (create buffer postscriptName))


(module+ test
  (require racket/runtime-path)
  (define-runtime-path charter-path "test/assets/charter.ttf")
  (openSync (path->string charter-path)))