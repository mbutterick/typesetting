#lang racket/base
(require pitfall/pdftest)

(define-runtime-path tiger "assets/tiger.json")

(define (proc doc)
  (send doc translate 220 300)
  (for* ([datum (in-list (read (open-input-string (string-replace (file->string tiger) #rx"[,:]" " "))))]
         [part (in-value (apply hash datum))])
    (send doc path (hash-ref part 'path))

    (when (hash-has-key? part "stroke-width")
      (send doc lineWidth (string->number (hash-ref part "stroke-width"))))

    (if (and (not (string=? (hash-ref part 'fill "none") "none"))
             (not (string=? (hash-ref part 'stroke "none") "none")))
        (send doc fillAndStroke (hash-ref part 'fill) (hash-ref part 'stroke))
        (begin
          (unless (string=? (hash-ref part 'fill "none") "none")
            (send doc fill (hash-ref part 'fill)))
          (unless (string=? (hash-ref part 'stroke "none") "none")
            (send doc fill (hash-ref part 'stroke)))))))

(define-runtime-path this "test10rkt.pdf")
(make-doc this #f proc #:pdfkit #f)

(define-runtime-path that "test10crkt.pdf")
(make-doc that #t proc #:pdfkit #f)
