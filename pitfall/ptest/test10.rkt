#lang racket/base
(require pitfall/pdftest racket/file racket/string)

(define-runtime-path tiger "assets/tiger.json")

(define (proc doc)
  (translate doc 220 300)
  (for* ([datum (in-list (read (open-input-string (string-replace (file->string tiger) #rx"[,:]" " "))))]
         [part (in-value (apply hash datum))])
    (path doc (hash-ref part 'path))

    (when (hash-has-key? part "stroke-width")
      (line-width doc (string->number (hash-ref part "stroke-width"))))

    (if (and (not (string=? (hash-ref part 'fill "none") "none"))
             (not (string=? (hash-ref part 'stroke "none") "none")))
        (fill-and-stroke doc (hash-ref part 'fill) (hash-ref part 'stroke))
        (begin
          (unless (string=? (hash-ref part 'fill "none") "none")
            (fill doc (hash-ref part 'fill)))
          (unless (string=? (hash-ref part 'stroke "none") "none")
            (fill doc (hash-ref part 'stroke)))))))

(define-runtime-path this "test10rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test10crkt.pdf")
(make-doc that #t proc)
