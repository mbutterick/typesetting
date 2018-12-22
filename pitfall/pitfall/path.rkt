#lang racket/base
(require
  racket/class
  racket/match
  racket/list
  brag/support
  sugar/list)
(provide parse-svg-path)

(define (parse-svg-path doc path)
  (define commands (parse path))
  (apply-commands commands doc))

(define (parse path)
  (define lex-1
    (lexer
     [(eof) eof]
     [alphabetic (string->symbol lexeme)]
     [(:: (:? "-") (:* numeric) (:? ".") (:+ numeric)) (string->number lexeme)]
     [(:or whitespace ",") (lex-1 input-port)]))
  (slicef-at (for/list ([tok (in-port lex-1 (open-input-string path))])
               tok) symbol?))

(module+ test
  (require rackunit)
  (check-equal?
   (parse "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90")
   '((M 0 20)
     (L 100 160)
     (Q 130 200 150 120)
     (C 190 -40 200 200 300 150)
     (L 400 90)))

  (check-equal?
   (parse "M-122.304 84.285C-122.304 84.285 -122.203 86.179 -123.027 86.16C-123.851 86.141 -140.305 38.066 -160.833 40.309C-160.833 40.309 -143.05 32.956 -122.304 84.285z")
   '((M -122.304 84.285)
     (C -122.304 84.285 -122.203 86.179 -123.027 86.16)
     (C -123.851 86.141 -140.305 38.066 -160.833 40.309)
     (C -160.833 40.309 -143.05 32.956 -122.304 84.285)
     (z)))

  (check-equal? (parse "L100-160") '((L 100 -160))))

(define (apply-commands commands doc)
  (for/fold ([cx 0][cy 0][px 0][py 0][sx 0][sy 0])
            ([cmd (in-list commands)])
    (match-define (cons cmd-name cmd-args) cmd)
    (let loop ([cmd-name cmd-name][cmd-args cmd-args])
      (match-define (list a0 a1 a2 a3 a4 a5)
        (append cmd-args (make-list (- 6 (length cmd-args)) #f)))
      (case cmd-name
        [(M) (send doc move-to . cmd-args)
             (values a0 a1 #f #f a0 a1)]
        [(m) (loop 'M (list (+ cx a0) (+ cy a1)))]
        [(C) (send doc bezier-curve-to . cmd-args)
             (values a4 a5 a2 a3 sx sy)]
        [(c) (loop 'C (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)
                            (+ cx a4) (+ cy a5)))]
        [(S) (match-let ([(list px py) (if (not px)
                                           (list cx cy)
                                           (list px py))])
               (send doc bezierCurveyTo (- cx (- px cx)) (- cy (- py cy)) a0 a1 a2 a3)
               (values a2 a3 a0 a1 sx sy))]
        [(s) (loop 'S (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)))]
        [(L) (send doc line-to . cmd-args)
             (values a0 a1 #f #f sx sy)]
        [(l) (loop 'L (list (+ cx a0) (+ cy a1)))]
        [(H) (loop 'L (list a0 cy))]
        [(h) (loop 'L (list (+ cx a0) cy))]
        [(V) (loop 'L (list cx a0))]
        [(v) (loop 'L (list cx (+ cy a0)))]
        [(Q) (send doc quadratic-curve-to . cmd-args)
             (values a2 a3 a0 a1 sx sy)]
        [(q) (loop 'Q (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)))]
        [(T) (match-define (list px py)
               (if (not px)
                   (list cx py)
                   (list (- cx (- px cx) (- cy (- py cy))))))
             (send doc quadratic-curve-to . cmd-args)]
        ;; todo other path ops
        [(z) (send doc close-path . cmd-args)
             (values sx sy px py sx sy)]
        [else (raise-argument-error 'apply-commands "valid command name" cmd-name)]))))