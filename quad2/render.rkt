#lang debug racket/base
(require "draw.rkt"
         "log.rkt"
         xml
         txexpr)
(provide (all-defined-out))

(module inner racket/base
  (provide (all-defined-out))
  (struct $renderer (doc-start-func
                     doc-end-func
                     page-start-func
                     page-end-func
                     text-func
                     set-font-func
                     move-func
                     return-func) #:transparent)

  (define-syntax-rule (check-arity PROCNAME [PROC ARITY] ...)
    (begin
      (unless (or (eq? (procedure-arity PROC) ARITY) (equal? void PROC))
        (raise-argument-error PROCNAME (format "procedure of arity ~a for ~a" ARITY 'PROC) PROC)) ...))

  (define (make-renderer
           #:doc-start-func [doc-start-func void]
           #:doc-end-func [doc-end-func void]
           #:page-start-func [page-start-func void]
           #:page-end-func [page-end-func void]
           #:text-func [text-func void]
           #:set-font-func [set-font-func void]
           #:move-func [move-func void]
           #:return-func [return-func void])
    (check-arity 'make-renderer
                 [doc-start-func 0]
                 [doc-end-func 0]
                 [page-start-func 2]
                 [page-end-func 0]
                 [text-func 1]
                 [set-font-func 1]
                 [move-func 2]
                 [return-func 0])
    ($renderer doc-start-func
               doc-end-func
               page-start-func
               page-end-func
               text-func
               set-font-func
               move-func
               return-func)))

(require 'inner)

(define null-renderer (make-renderer))

(define current-renderer (make-parameter null-renderer))

(define text-renderer
  ;; scan over the instructions and record where the chars want to go
  (let ([char-pos-table (make-hasheqv)]
        [current-loc 0+0i]
        [xmax 0]
        [ymax 0]
        [results null])
    (make-renderer
     #:page-start-func
     (λ (width height)
       (set! xmax width)
       (set! ymax height))
     #:page-end-func
     (λ ()
       ;; fill in a character grid
       (define str (string-join
                    (for/list ([y (in-range ymax)])
                      (list->string
                       (map integer->char
                            (for/list ([x (in-range xmax)])
                              (hash-ref char-pos-table (make-rectangular x y) (char->integer #\space)))))) "\n"))
       (set! results (cons str results)))
     #:text-func (λ (str) (hash-set! char-pos-table current-loc str))
     #:move-func (λ (x y) (set! current-loc (make-rectangular x y)))
     #:return-func (λ ()
                     #;(unless (pair? results)
                         (error 'text-renderer-failed))
                     (for-each displayln results)))))

(require racket/gui)

(define drr-renderer
  ;; scan over the instructions and record where the chars want to go
  (let ([targets null]
        [dc #f]
        [current-loc 0+0i]
        [current-font #false])
    (make-renderer
     #:page-start-func
     (let ([my-face (match (get-face-list 'mono)
                      [(? null?) (error 'no-mono-font-available)]
                      [(cons face _) face])])
       (λ (width height)
         (define target (make-bitmap width height))
         (set! targets (cons target targets))
         (set! dc (new bitmap-dc% [bitmap target]))
         (send dc set-font (make-font #:size 1 #:face my-face))
         (send dc set-text-foreground "black")))
     #:text-func
     (λ (charint)
       (when dc
         (send dc draw-text (string (integer->char charint)) (real-part current-loc) (imag-part current-loc))))
     #:set-font-func
     (λ (ps)
       ;; racket/draw can't load arbitrary user fonts from a path
       ;; https://github.com/racket/racket/issues/1348
       ;; TODO: font substitution? but this would require
       ;; holding & propagating Panose-like metadata about the font
       ;; but it would allow slightly more accurate rendering for contexts
       ;; that don't support fonts by path
       (log-quad2-warning (format "can't load font ~a" ps)))
     #:move-func
     (λ (x y) (set! current-loc (make-rectangular x y)))
     #:return-func
     (λ () (for-each displayln (map (λ (target) (make-object image-snip% target)) targets))))))

(define (html-renderer html-file)
  (let ([xmax 0]
        [ymax 0]
        [page-quads null]
        [current-loc 0+0i]
        [pages null]
        [fonts (make-hasheqv)]
        [current-font ""])
    (make-renderer
     #:page-start-func
     (λ (width height)
       (set! page-quads null)
       (set! xmax width)
       (set! ymax height))
     #:page-end-func
     (λ ()
       (set! pages (cons `(div ((class "page")
                                (style ,(format "position: relative;width:~apx;height:~apx;border:1px solid black;background:white" xmax ymax))) ,@(reverse page-quads)) pages))
       (set! page-quads null))
     #:text-func
     (λ (charint)
       (set! page-quads (cons
                         `(div ((style ,(format "position: absolute;left:~apx;top:~apx;font-family:~a;font-size:~apx" (real-part current-loc) (imag-part current-loc) current-font 12)))
                               ,(string (integer->char charint))) page-quads)))
     #:set-font-func
     (λ (ps)
       (set! current-font (hash-ref! fonts ps (λ () (gensym 'font)))))
     #:move-func
     (λ (x y) (set! current-loc (make-rectangular x y)))
     #:return-func
     (λ ()
       (with-output-to-file html-file
         #:exists 'replace
         (λ ()
           (displayln "<!DOCTYPE html>")
           (display-xml/content
            (xexpr->xml `(html
                          (head (style ((type "text/css"))
                                       ,(string-join
                                         (for/list ([(ps fontsym) (in-hash fonts)])
                                           (format "@font-face { font-family: \"~a\";\nsrc: url(\"~a\");}" fontsym ps)))))
                          (body ((style "background: #ddd"))
                                ,@pages))))))))))

(define-syntax (cond-eq stx)
  (syntax-case stx (else)
    [(MAC ARG [SYM . BODY] ... [else ELSEBODY])
     #'(cond
         [(eq? ARG SYM) . BODY] ...
         [else ELSEBODY])]
    [(MAC ARG CLAUSE ...)
     #'(MAC ARG CLAUSE ... [else (void)])]))

(define (render inst-str #:using [renderer (current-renderer)])
  (match-define ($renderer
                 doc-start-func
                 doc-end-func
                 page-start-func
                 page-end-func
                 text-func
                 set-font-func
                 move-func
                 return-func) renderer)
  (let/ec exit
    (for/fold ([stack null]
               #:result (void))
              ([tok (in-port read (open-input-string inst-str))])
      (cond-eq tok
               ['doc-start (doc-start-func) stack]
               ['doc-end (exit (doc-end-func)) (error 'should-never-reach)]
               ['page-start
                (match-define (list* x y tail) stack)
                (page-start-func x y)
                tail]
               ['page-end (page-end-func) stack]
               ['text
                (match-define (cons charint tail) stack)
                (text-func charint)
                tail]
               ['set-font
                (match-define (cons path-string tail) stack)
                (set-font-func (symbol->string path-string))
                tail]
               ['move
                (match-define (list* x y tail) stack)
                (move-func x y)
                tail]
               [else (cons tok stack)])))
  (return-func))

