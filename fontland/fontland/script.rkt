#lang racket/base
(require sugar/unstable/dict
         sugar/unstable/contract
         sugar/unstable/stub
         racket/contract)

(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/layout/Script.js
|#

;; This maps the Unicode Script property to an OpenType script tag
;; Data from http://www.microsoft.com/typography/otspec/scripttags.htm
;; and http://www.unicode.org/Public/UNIDATA/PropertyValueAliases.txt.

(define UNICODE_SCRIPTS
  (apply mhash
         '(Caucasian_Albanian aghb
                              Arabic arab
                              Imperial_Aramaic armi
                              Armenian armn
                              Avestan avst
                              Balinese bali
                              Bamum bamu
                              Bassa_Vah bass
                              Batak batk
                              Bengali '(bng2 beng)
                              Bopomofo bopo
                              Brahmi brah
                              Braille brai
                              Buginese bugi
                              Buhid buhd
                              Chakma cakm
                              Canadian_Aboriginal cans
                              Carian cari
                              Cham cham
                              Cherokee cher
                              Coptic copt
                              Cypriot cprt
                              Cyrillic cyrl
                              Devanagari '(dev2 deva)
                              Deseret dsrt
                              Duployan dupl
                              Egyptian_Hieroglyphs egyp
                              Elbasan elba
                              Ethiopic ethi
                              Georgian geor
                              Glagolitic glag
                              Gothic goth
                              Grantha gran
                              Greek grek
                              Gujarati '(gjr2 gujr)
                              Gurmukhi '(gur2 guru)
                              Hangul hang
                              Han hani
                              Hanunoo hano
                              Hebrew hebr
                              Hiragana hira
                              Pahawh_Hmong hmng
                              Katakana_Or_Hiragana hrkt
                              Old_Italic ital
                              Javanese java
                              Kayah_Li kali
                              Katakana kana
                              Kharoshthi khar
                              Khmer khmr
                              Khojki khoj
                              Kannada '(knd2 knda)
                              Kaithi kthi
                              Tai_Tham lana
                              Lao lao 
                              Latin latn
                              Lepcha lepc
                              Limbu limb
                              Linear_A lina
                              Linear_B linb
                              Lisu lisu
                              Lycian lyci
                              Lydian lydi
                              Mahajani mahj
                              Mandaic mand
                              Manichaean mani
                              Mende_Kikakui mend
                              Meroitic_Cursive merc
                              Meroitic_Hieroglyphs mero
                              Malayalam '(mlm2 mlym)
                              Modi modi
                              Mongolian mong
                              Mro mroo
                              Meetei_Mayek mtei
                              Myanmar '(mym2 mymr)
                              Old_North_Arabian narb
                              Nabataean nbat
                              Nko nko 
                              Ogham ogam
                              Ol_Chiki olck
                              Old_Turkic orkh
                              Oriya orya
                              Osmanya osma
                              Palmyrene palm
                              Pau_Cin_Hau pauc
                              Old_Permic perm
                              Phags_Pa phag
                              Inscriptional_Pahlavi phli
                              Psalter_Pahlavi phlp
                              Phoenician phnx
                              Miao plrd
                              Inscriptional_Parthian prti
                              Rejang rjng
                              Runic runr
                              Samaritan samr
                              Old_South_Arabian sarb
                              Saurashtra saur
                              Shavian shaw
                              Sharada shrd
                              Siddham sidd
                              Khudawadi sind
                              Sinhala sinh
                              Sora_Sompeng sora
                              Sundanese sund
                              Syloti_Nagri sylo
                              Syriac syrc
                              Tagbanwa tagb
                              Takri takr
                              Tai_Le tale
                              New_Tai_Lue talu
                              Tamil taml
                              Tai_Viet tavt
                              Telugu '(tel2 telu)
                              Tifinagh tfng
                              Tagalog tglg
                              Thaana thaa
                              Thai thai
                              Tibetan tibt
                              Tirhuta tirh
                              Ugaritic ugar
                              Vai vai 
                              Warang_Citi wara
                              Old_Persian xpeo
                              Cuneiform xsux
                              Yi yi
                              Inherited zinh
                              Common zyyy
                              Unknown zzzz)))


(define/contract (fromUnicode script)
  ((option/c symbol?) . -> . symbol?)
  (hash-ref UNICODE_SCRIPTS script #f))

(define-stub-stop forString)

(define-stub-stop forCodePoints)

(define RTL '(  arab ;; Arabic
  hebr ;; Hebrew
  syrc ;; Syriac
  thaa ;; Thaana
  cprt ;; Cypriot Syllabary
  khar ;; Kharosthi
  phnx ;; Phoenician
 |nko | ;; N'Ko
  lydi ;; Lydian
  avst ;; Avestan
  armi ;; Imperial Aramaic
  phli ;; Inscriptional Pahlavi
  prti ;; Inscriptional Parthian
  sarb ;; Old South Arabian
  orkh ;; Old Turkic, Orkhon Runic
  samr ;; Samaritan
  mand ;; Mandaic, Mandaean
  merc ;; Meroitic Cursive
  mero ;; Meroitic Hieroglyphs

  ;; Unicode 7.0 (not listed on http://www.microsoft.com/typography/otspec/scripttags.htm)
  mani ;; Manichaean
  mend ;; Mende Kikakui
  nbat ;; Nabataean
  narb ;; Old North Arabian
  palm ;; Palmyrene
  phlp ;; Psalter Pahlavi
  ))

(define/contract (direction script)
  ((option/c symbol?) . -> . (or/c 'rtl 'ltr))
  (if (memq script RTL) 'rtl 'ltr))