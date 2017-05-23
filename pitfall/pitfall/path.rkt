#lang pitfall/racket
(provide parse-svg-path)

(define (parse-svg-path doc path)
  (define commands (parse path))
  (apply-commands commands doc))

(define (parse path)
  (for/list ([str (in-list (string-split (string-replace path "," " ") #px"(?=[A-Za-z])"))]
             #:when (not (string=? str "")))
    (read (open-input-string (string-append "(" str ")")))))

(module+ test
  (require rackunit)
  (check-equal?
   (parse "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90")
   '((M 0 20)
     (L 100 160)
     (Q 130 200 150 120)
     (C 190 -40 200 200 300 150)
     (L 400 90))))

(define (apply-commands commands doc)
  (for/fold ([cx 0][cy 0][px 0][py 0][sx 0][sy 0])
            ([cmd (in-list commands)])
    (match-define (cons cmd-name cmd-args) cmd)
    (let loop ([cmd-name cmd-name][cmd-args cmd-args])
      (case cmd-name
        [(M) (send doc moveTo . cmd-args)
             (match-define (list a0 a1) cmd-args)
             (values a0 a1 #f #f a0 a1)]
        [(C) (send doc bezierCurveTo . cmd-args)
             (match-define (list a0 a1 a2 a3 a4 a5) cmd-args)
             (values a4 a5 a2 a3 sx sy)]
        [(L) (send doc lineTo . cmd-args)
             (match-define (list a0 a1) cmd-args)
             (values a0 a1 #f #f sx sy)]
        [(Q) (send doc quadraticCurveTo . cmd-args)
             (match-define (list a0 a1 a2 a3) cmd-args)
             (values a2 a3 a0 a1 sx sy)]
        [else (values cx cy px py sx sy)]))))