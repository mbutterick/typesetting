#lang pitfall/racket
(provide parse-svg-path)

(define (parse-svg-path doc path)
  (define commands (parse path))
  (apply-commands commands doc))

(define (parse path)
  (let* ([path (string-replace path "," " ")] ; no commas
         [path (string-replace path "-" " -")] ; at least one space before negative signs
         [path (string-replace path  #px"(?<=[A-Za-z])" " ")]) ; at least one space after letters
    (for/list ([str (in-list (string-split path #px"(?=[A-Za-z])"))]
               #:unless (zero? (string-length str)))
      (read (open-input-string (format "(~a)" str))))))

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
      (case cmd-name
        [(M) (send doc moveTo . cmd-args)
             (match-define (list a0 a1) cmd-args)
             (values a0 a1 #f #f a0 a1)]
        [(m) (match-define (list a0 a1) cmd-args)
             (loop 'M (list (+ cx a0) (+ cy a1)))]
        [(C) (send doc bezierCurveTo . cmd-args)
             (match-define (list a0 a1 a2 a3 a4 a5) cmd-args)
             (values a4 a5 a2 a3 sx sy)]
        [(c) (match-define (list a0 a1 a2 a3 a4 a5) cmd-args)
             (loop 'C (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)
                            (+ cx a4) (+ cy a5)))]
        [(L) (send doc lineTo . cmd-args)
             (match-define (list a0 a1) cmd-args)
             (values a0 a1 #f #f sx sy)]
        [(l) (match-define (list a0 a1) cmd-args)
             (loop 'L (list (+ cx a0) (+ cy a1)))]
        [(H) (match-define (list a0) cmd-args)
             (loop 'L (list a0 cy))]
        [(h) (match-define (list a0) cmd-args)
             (loop 'L (list (+ cx a0) cy))]
        [(V) (match-define (list a0) cmd-args)
             (loop 'L (list cx a0))]
        [(v) (match-define (list a0) cmd-args)
             (loop 'L (list cx (+ cy a0)))]
             
        [(Q) (send doc quadraticCurveTo . cmd-args)
             (match-define (list a0 a1 a2 a3) cmd-args)
             (values a2 a3 a0 a1 sx sy)]
        [(z) (send doc closePath . cmd-args)
             (values sx sy px py sx sy)]
        [else (raise-argument-error 'apply-commands "valid command name" cmd-name)]))))