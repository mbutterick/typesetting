#lang debug racket
(require "hacs.rkt" sugar/debug)
(module+ test (require rackunit))

(define (map-coloring-csp colors neighbors)
  (define names (remove-duplicates (flatten neighbors) eq?))
  (define vds (for/list ([name (in-list names)])
                (var name colors)))
  (define cs (for*/list ([neighbor neighbors]
                         [target (cdr neighbor)])
               (constraint (list (car neighbor) target) neq?)))
  (csp vds cs))

(define (parse-colors str) (map string->symbol (map string-downcase (regexp-match* "." str))))
(define(parse-neighbors str)
  (define recs (map string-trim (string-split str ";")))
  (for/list ([rec recs])
    (match-define (cons head tail) (string-split rec ":"))
    (map string->symbol (map string-downcase (map string-trim (cons head (string-split (if (pair? tail)
                                                                                           (car tail)
                                                                                           ""))))))))
  
(current-inference forward-check)
(current-select-variable minimum-remaining-values)
(current-order-values shuffle)

(define aus (map-coloring-csp (parse-colors "RGB")
                              (parse-neighbors "SA: WA NT Q NSW V; NT: WA Q; NSW: Q V; T: ")))

(module+ test
  (check-equal? (length (solve* aus)) 18))

(define usa (map-coloring-csp (parse-colors "RGBY")
                              (parse-neighbors "WA: OR ID; OR: ID NV CA; CA: NV AZ; NV: ID UT AZ; ID: MT WY UT;
        UT: WY CO AZ; MT: ND SD WY; WY: SD NE CO; CO: NE KA OK NM; NM: OK TX;
        ND: MN SD; SD: MN IA NE; NE: IA MO KA; KA: MO OK; OK: MO AR TX;
        TX: AR LA; MN: WI IA; IA: WI IL MO; MO: IL KY TN AR; AR: MS TN LA;
        LA: MS; WI: MI IL; IL: IN KY; IN: OH KY; MS: TN AL; AL: TN GA FL;
        MI: OH IN; OH: PA WV KY; KY: WV VA TN; TN: VA NC GA; GA: NC SC FL;
        PA: NY NJ DE MD WV; WV: MD VA; VA: MD DC NC; NC: SC; NY: VT MA CT NJ;
        NJ: DE; DE: MD; MD: DC; VT: NH MA; MA: NH RI CT; CT: RI; ME: NH;
        HI: ; AK:")))

(module+ test
  (check-true (pair? (solve usa))))

(define fr (map-coloring-csp (parse-colors "RGBY")
                             (parse-neighbors "AL: LO FC; AQ: MP LI PC; AU: LI CE BO RA LR MP; BO: CE IF CA FC RA
        AU; BR: NB PL; CA: IF PI LO FC BO; CE: PL NB NH IF BO AU LI PC; FC: BO
        CA LO AL RA; IF: NH PI CA BO CE; LI: PC CE AU MP AQ; LO: CA AL FC; LR:
        MP AU RA PA; MP: AQ LI AU LR; NB: NH CE PL BR; NH: PI IF CE NB; NO:
        PI; PA: LR RA; PC: PL CE LI AQ; PI: NH NO CA IF; PL: BR NB CE PC; RA:
        AU BO FC PA LR")))

(module+ test
(check-true (pair? (solve fr))))

(module+ main)