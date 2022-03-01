#lang debug racket/base
(require racket/list
         racket/string
         racket/format
         racket/match
         "quad.rkt"
         "pipeline.rkt"
         "struct.rkt"
         "layout.rkt")
(provide (all-defined-out))

(define-pass (make-drawing-insts qs)
  #:pre (list-of has-position?)
  #:post (list-of $drawing-inst?)
  (flatten
   (list ($doc 'start) ($page 'start)
         (for/list ([q (in-list qs)])
                   (cond
                     [(quad? q)
                      (list ($move (quad-posn q)) ($text (char->integer (quad-char q))))]
                     [else (error 'render-unknown-thing)]))
         ($page 'end) ($doc 'end))))

(define valid-tokens '(doc-start doc-end page-start page-end text move))

(define-pass (stackify xs)
  #:pre (list-of $drawing-inst?)
  #:post string?
  (define move-points (map $move-posn (filter $move? xs)))
  (define xmax (add1 (apply max (map $point-x move-points))))
  (define ymax (add1 (apply max (map $point-y move-points))))
  (string-join
   (for/list ([x (in-list xs)])
             (string-join (map ~a (match x
                                    [($move ($point x y)) (list y x 'move)]
                                    [($text charint) (list charint 'text)]
                                    [($doc 'start) '(doc-start)]
                                    [($doc 'end) '(doc-end)]
                                    [($page 'start) (list ymax xmax 'page-start)]
                                    [($page 'end) '(page-end)]
                                    [_ (error 'unknown-drawing-inst)])) " ")) "\n"))