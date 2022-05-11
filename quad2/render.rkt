#lang debug racket/base
(require "pipeline.rkt"
         "draw.rkt"
         "layout.rkt"
         "log.rkt"
         "glyphrun.rkt")
(provide (all-defined-out))

(struct $renderer (doc-start-func
                   doc-end-func
                   page-start-func
                   page-end-func
                   text-func
                   move-func
                   return-func
                   set-font-func) #:transparent)

(define current-renderer (make-parameter ($renderer void void void void void void void void)))

(define text-renderer
  ;; scan over the instructions and record where the chars want to go
  (let ([char-pos-table (make-hasheqv)]
        [current-loc 0+0i]
        [xmax 0]
        [ymax 0]
        [results null])
    ($renderer
     void
     void
     (λ (width height)
       (set! xmax width)
       (set! ymax height))
     (λ ()
       ;; fill in a character grid
       (define str (string-join
                    (for/list ([y (in-range ymax)])
                              (list->string
                               (map integer->char
                                    (for/list ([x (in-range xmax)])
                                              (hash-ref char-pos-table (make-rectangular x y) (char->integer #\space)))))) "\n"))
       (set! results (cons str results)))
     (λ (str) (hash-set! char-pos-table current-loc str))
     (λ (x y) (set! current-loc (make-rectangular x y)))
     (λ ()
       (unless (pair? results)
         (error 'text-renderer-failed))
       (for-each displayln results))
     void)))

(require racket/gui)

(define drr-renderer
  ;; scan over the instructions and record where the chars want to go
  (let ([targets null]
        [dc #f]
        [current-loc 0+0i]
        [current-font #false])
    ($renderer
     void
     void
     (let ([em-scale 30]
           [my-face (match (get-face-list 'mono)
                      [(? null?) (error 'no-mono-font-available)]
                      [(cons face _) face])])
       (λ (width height)
         (define target (make-bitmap (* em-scale width) (* em-scale height)))
         (set! targets (cons target targets))
         (set! dc (new bitmap-dc% [bitmap target]))
         (send dc scale em-scale em-scale) 
         (send dc set-font (make-font #:size 1 #:face my-face))
         (send dc set-text-foreground "black")))
     void
     (λ (charint)
       (send dc draw-text (string (integer->char charint)) (real-part current-loc) (imag-part current-loc)))
     (λ (x y) (set! current-loc (make-rectangular x y)))
     (λ () (for-each displayln (map (λ (target) (make-object image-snip% target)) targets)))
     (λ (ps)
       ;; racket/draw can't load arbitrary user fonts from a path
       ;; https://github.com/racket/racket/issues/1348
       ;; TODO: font substitution? but this would require
       ;; holding & propagating Panose-like metadata about the font
       ;; but it would allow slightly more accurate rendering for contexts
       ;; that don't support fonts by path
       (log-quad2-warning (format "can't load font ~a" ps))))))

(define (render inst-str #:using [renderer (current-renderer)])
  (let/ec exit
    (for/fold ([stack null]
               #:result (void))
              ([tok (in-port read (open-input-string inst-str))])
      (define next-stack (cons tok stack))
      (cond
        [(memq tok valid-tokens)
         (match next-stack
           [(list* 'doc-start rest) (($renderer-doc-start-func renderer)) rest]
           [(list* 'doc-end _) (exit (($renderer-doc-end-func renderer)))]
           [(list* 'page-start x y rest) (($renderer-page-start-func renderer) x y) rest]
           [(list* 'page-end rest) (($renderer-page-end-func renderer)) rest]
           [(list* 'text charint rest) (($renderer-text-func renderer) charint) rest]
           [(list* 'set-font path-string rest) (($renderer-set-font-func renderer) (symbol->string path-string)) rest]
           [(list* 'move x y rest) (($renderer-move-func renderer) x y) rest]
           [_ next-stack])]
        [else next-stack])))
  (($renderer-return-func renderer)))

