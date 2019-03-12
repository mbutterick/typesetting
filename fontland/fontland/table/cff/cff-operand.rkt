#lang debug racket/base
(require racket/class xenomorph "cff-struct.rkt")
(provide CFFOperand)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFOperand.js
|#

(define FLOAT_EOF #xf)

(define FLOAT_LOOKUP (vector "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "." "E" "E-" "" "-"))

(define FLOAT_ENCODE_LOOKUP
  (hash "." 10
          "E" 11
          "E-" 12
          "-" 14))

(define CFFOperand%
  (class x:base%
    (super-new)

    (augment [@decode decode])
    (define (@decode stream _ value)
      (cond
        [(<= 32 value 246) (- value 139)]
        [(<= 247 value 250) (+ (* (- value 247) 256) (read-byte stream) 108)]
        [(<= 251 value 254) (- (* (- (- value 251)) 256) (read-byte stream) 108)]
        [(= value 28) (decode int16be stream)]
        [(= value 29) (decode int32be stream)]
        [(= value 30)
         (for/fold ([strs null]
                    [break? #false]
                    #:result (* (string->number (apply string-append (reverse strs))) 1.0))
                   ([i (in-naturals)]
                    #:break break?)
           (define b (read-byte stream))

           (define n1 (arithmetic-shift b -4))
           
           (cond
             [(= n1 FLOAT_EOF) (values strs 'break-now)]
             [else
              (let ([strs (cons (vector-ref FLOAT_LOOKUP n1) strs)])
                (define n2 (bitwise-and b 15))
                (cond
                  [(= n2 FLOAT_EOF) (values strs 'break-now)]
                  [else
                   (let ([strs (cons (vector-ref FLOAT_LOOKUP n2) strs)])
                     (values strs #false))]))]))]))

    (define/augment (size value-arg _)
      ;; if the value needs to be forced to the largest size (32 bit)
      ;; e.g. for unknown pointers, set to 32768
      (define value (cond
                      [(or (and (hash? value-arg) (hash-ref value-arg 'forceLarge #f))
                           (and (Ptr? value-arg) (Ptr-forceLarge value-arg)))
                       32768]
                      [(Ptr? value-arg) (Ptr-val value-arg)]
                      [else value-arg]))

      (cond
        [(not (integer? value)) ; floating point
         (define str (number->string value))
         (add1 (ceiling (/ (add1 (string-length str)) 2)))]
        [(<= -107 value 107) 1]
        [(<= -1131 value 1131) 2]
        [(<= -32768 value 32767) 3]
        [else 5]))

    (augment [@encode encode])
    (define (@encode value-arg stream . _)
      ;; if the value needs to be forced to the largest size (32 bit)
      ;; e.g. for unknown pointers, save the old value and set to 32768
      (define value (if (Ptr? value-arg) (Ptr-val value-arg) value-arg))
      (define val (if value (string->number (format "~a" value)) 0))

      (cond
        [(and (Ptr? value-arg) (Ptr-forceLarge value-arg))
         (encode uint8 29 stream)
         (encode int32be val stream)]
        [(not (integer? val)) ;; floating point
         (encode uint8 30 stream)
         (define str (list->vector (regexp-match* #rx"." (number->string val))))
         (define n2 'nothing)
         (for ([i (in-range 0 (vector-length str) 2)])
           (define c1 (vector-ref str i))
           (define n1 (hash-ref FLOAT_ENCODE_LOOKUP c1 (string->number c1)))

           (cond
             [(= i (sub1 (vector-length str)))
              (set! n2 FLOAT_EOF)]
             [else
              (define c2 (vector-ref str (add1 i)))
              (set! n2 (hash-ref FLOAT_ENCODE_LOOKUP c2 (string->number c2)))])

           (encode uint8 (bitwise-ior (arithmetic-shift n1 -4) (bitwise-and n2 15)) stream))
                    
         (unless (= n2 FLOAT_EOF)
           (encode uint8 (arithmetic-shift FLOAT_EOF 4) stream))]
        [(<= -107 value 107)
         (encode uint8 (+ val 139) stream)]
        [(<= 108 value 1131)
         (encode uint8 (+ (arithmetic-shift val -8) 247) stream)
         (encode uint8 (bitwise-and val #xff) stream)]
        [(<= -1131 value -108)
         (let ([val (- (- val) 108)])
           (encode uint8 (+ (arithmetic-shift val -8) 251) stream)
           (encode uint8 (bitwise-and val #xff) stream))]
        [(<= -32768 value 32767)
         (encode uint8 28 stream)
         (encode uint16be val stream)]
        [else
         (encode uint8 29 stream)
         (encode uint32be val stream)]))))


(define CFFOperand (make-object CFFOperand%))