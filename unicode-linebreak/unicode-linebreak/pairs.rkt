#lang at-exp debug racket/base
(require racket/string racket/match)
(provide (all-defined-out))

(define DI_BRK 0) ; Direct break opportunity
(define IN_BRK 1) ; Indirect break opportunity
(define CI_BRK 2) ; Indirect break opportunity for combining marks
(define CP_BRK 3) ; Prohibited break for combining marks
(define PR_BRK 4) ; Prohibited break

(define/match (tok->val tok)
    [("^") PR_BRK]
    [("@") CP_BRK]
    [("_") DI_BRK]
    [("%") IN_BRK]
    [("#") CI_BRK])

(define/match (make-pair-table . strs)
  [((cons header recs))
   (for/list ([rec (in-list recs)]
              #:unless (regexp-match #px"^\\s*$" rec))
     (for/list ([tok (in-list (string-split (string-trim rec #px"\\w+")))])
       (tok->val tok)))])

(define/match (make-pair-hash . strs)
  [((list* header _ recs))
   (define tok-pat #px"\\w+")
   (for*/hash ([right (in-list (map string->symbol (string-split header)))]
               [rec (in-list recs)]
               #:unless (regexp-match #px"^\\s*$" rec)
               [left (in-value (string->symbol (car (regexp-match tok-pat rec))))]
               [tok (in-list (string-split (string-trim rec tok-pat)))])
     (values (cons left right) (tok->val tok)))])

;; table copied from https://www.unicode.org/reports/tr14/tr14-37.html#Table2
(define table-strs
  @list|{
 OP  CL  CP  QU  GL  NS  EX  SY  IS  PR  PO  NU  AL  HL  ID  IN  HY  BA  BB  B2  ZW  CM  WJ  H2  H3  JL  JV  JT  RI  EB  EM  ZWJ
 OP  ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   @   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^
 CL  _   ^   ^   %   %   ^   ^   ^   ^   %   %   _   _   _   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 CP  _   ^   ^   %   %   ^   ^   ^   ^   %   %   %   %   %   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 QU  ^   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   %   %   %   %   %   %   ^   #   ^   %   %   %   %   %   %   %   %   %
 GL  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   %   %   %   %   %   %   ^   #   ^   %   %   %   %   %   %   %   %   %
 NS  _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 EX  _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 SY  _   ^   ^   %   %   %   ^   ^   ^   _   _   %   _   %   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 IS  _   ^   ^   %   %   %   ^   ^   ^   _   _   %   %   %   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 PR  %   ^   ^   %   %   %   ^   ^   ^   _   _   %   %   %   %   _   %   %   _   _   ^   #   ^   %   %   %   %   %   _   %   %   %
 PO  %   ^   ^   %   %   %   ^   ^   ^   _   _   %   %   %   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 NU  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 AL  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 HL  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 ID  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 IN  _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 HY  _   ^   ^   %   _   %   ^   ^   ^   _   _   %   _   _   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 BA  _   ^   ^   %   _   %   ^   ^   ^   _   _   _   _   _   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 BB  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   %   %   %   %   %   %   ^   #   ^   %   %   %   %   %   %   %   %   %
 B2  _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   _   _   %   %   _   ^   ^   #   ^   _   _   _   _   _   _   _   _   %
 ZW  _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   ^   _   _   _   _   _   _   _   _   _   _   _
 CM  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 WJ  %   ^   ^   %   %   %   ^   ^   ^   %   %   %   %   %   %   %   %   %   %   %   ^   #   ^   %   %   %   %   %   %   %   %   %
 H2  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   %   %   _   _   _   %
 H3  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   %   _   _   _   %
 JL  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   %   %   %   %   _   _   _   _   %
 JV  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   %   %   _   _   _   %
 JT  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   %   _   _   _   %
 RI  _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   _   _   %   %   _   _   ^   #   ^   _   _   _   _   _   %   _   _   %
 EB  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   %   %
 EM  _   ^   ^   %   %   %   ^   ^   ^   _   %   _   _   _   _   %   %   %   _   _   ^   #   ^   _   _   _   _   _   _   _   _   %
 ZWJ _   ^   ^   %   %   %   ^   ^   ^   _   _   _   _   _   %   _   %   %   _   _   ^   #   ^   _   _   _   _   _   _   %   %   %
 }|)
(define pair-table (apply make-pair-table table-strs))
(define pair-hash (apply make-pair-hash table-strs))

(module+ main
  pair-table)