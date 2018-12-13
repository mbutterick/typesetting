#lang racket/base
(require rackunit
         racket/match
         racket/list
         sugar/unstable/dict
         "../helper.rkt"
         "../number.rkt"
         "../bitfield.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Bitfield.coffee
|#

(define bitfield (+xbitfield uint8 '(Jack Kack Lack Mack Nack Oack Pack Quack)))
(match-define (list JACK KACK LACK MACK NACK OACK PACK QUACK)
  (map (λ (x) (arithmetic-shift 1 x)) (range 8)))

(test-case
 "bitfield should have the right size"
 (check-equal? (size bitfield) 1))

(test-case
 "bitfield should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes (bitwise-ior JACK MACK PACK NACK QUACK)))])
   (check-equal? (decode bitfield) (mhasheq 'Quack #t
                                            'Nack #t
                                            'Lack #f
                                            'Oack #f
                                            'Pack #t
                                            'Mack #t
                                            'Jack #t
                                            'Kack #f))))

(test-case
 "bitfield should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes (bitwise-ior JACK MACK PACK NACK QUACK)))])
   (set-post-decode! bitfield (λ (fh . _) (hash-set! fh 'foo 42) fh))
   (check-equal? (decode bitfield) (mhasheq 'Quack #t
                                            'Nack #t
                                            'Lack #f
                                            'Oack #f
                                            'Pack #t
                                            'Mack #t
                                            'Jack #t
                                            'Kack #f
                                            'foo 42))))

(test-case
 "bitfield should encode"
 (check-equal? (encode bitfield (mhasheq 'Quack #t
                                         'Nack #t
                                         'Lack #f
                                         'Oack #f
                                         'Pack #t
                                         'Mack #t
                                         'Jack #t
                                         'Kack #f) #f)
               (bytes (bitwise-ior JACK MACK PACK NACK QUACK))))

(test-case
 "bitfield should encode with pre-encode"
 (set-pre-encode! bitfield (λ (fh . _)
                             (hash-set! fh 'Jack #f)
                             (hash-set! fh 'Mack #f)
                             (hash-set! fh 'Pack #f)
                             fh))
 (check-equal? (encode bitfield (mhasheq 'Quack #t
                                         'Nack #t
                                         'Lack #f
                                         'Oack #f
                                         'Pack #t
                                         'Mack #t
                                         'Jack #t
                                         'Kack #f) #f)
               (bytes (bitwise-ior NACK QUACK))))
