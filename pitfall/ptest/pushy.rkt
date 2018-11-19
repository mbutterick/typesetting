#lang debug br
(require racket/bytes)


(define (outer-delimiter bs left right)
  (parameterize ([current-input-port (open-input-bytes bs)])
    (let loop ([acc null][depth 0])
      (cond
        [(regexp-try-match (regexp (format "^~a" left)) (current-input-port))
         => (λ (m) (loop (cons (car m) acc) (add1 depth)))]
        [(regexp-try-match (regexp (format "^~a" right)) (current-input-port)) 
         => (λ (m)
              (case depth
                [(= depth 1) (apply bytes-append (reverse (cons (car m) acc)))]
                [else (loop (cons (car m) acc) (sub1 depth))]))]
        [else
         (define b (read-byte))
         (loop (if (empty? acc)
                   acc ; drop leading non-delimiter bytes
                   (cons (bytes b) acc)) depth)]))))

(define bs #"aaa<<bbb<<ccc>>xxx<<zzz>>ddd>>eee<<fff>>ggg")

(outer-delimiter bs #"<<" #">>")