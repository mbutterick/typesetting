#lang debug racket/base
(require racket/list
         racket/string
         racket/format
         racket/match
         "quad.rkt"
         "pipeline.rkt"
         "struct.rkt"
         "layout.rkt"
         "constants.rkt")
(provide (all-defined-out))

(define-pass (make-drawing-insts qs)
  #:pre (list-of has-position?)
  #:post (list-of $drawing-inst?)
  (apply append
         (let ([current-font #false])
           (for/list ([q (in-list qs)])
                     (cond
                       [(eq? boq q) (list ($doc-start))]
                       [(eq? eoq q) (list ($doc-end))]
                       [(bop-quad? q) (list ($page-start (quad-ref q :page-width) (quad-ref q :page-height)))]
                       [(eop-quad? q) (list ($page-end))]
                       [(quad? q)
                        (append
                         (match (quad-ref q :font-path)
                           [(== current-font) null]
                           [font-path
                            (set! current-font font-path)
                            (list ($font font-path))])
                         (if (pair? (quad-elems q))
                             (list ($move (quad-posn q)) ($text (char->integer (car (string->list (car (quad-elems q)))))))
                             null))]
                       [else (error 'render-unknown-thing)])))))

(define valid-tokens '(doc-start doc-end page-start page-end text move set-font))

(define-pass (stackify xs)
  #:pre (list-of $drawing-inst?)
  #:post string?
  (define move-points (map $move-posn (filter $move? xs)))
  (define xmax (if (pair? move-points) (add1 (apply max (map $point-x move-points))) 0))
  (define ymax (if (pair? move-points) (add1 (apply max (map $point-y move-points))) 0))
  (string-join
   (for/list ([x (in-list xs)])
             (string-join (map ~a (match x
                                    ;; TODO: embed these code-generating functions
                                    ;; as properties of the structs
                                    [($move ($point x y)) (list y x 'move)]
                                    [($text charint) (list charint 'text)]
                                    [($font path-string) (list path-string 'set-font)]
                                    [($doc-start) '(doc-start)]
                                    [($doc-end) '(doc-end)]
                                    [($page-start width height) (list height width 'page-start)]
                                    [($page-end) '(page-end)]
                                    [_ (error 'unknown-drawing-inst)])) " ")) "\n"))