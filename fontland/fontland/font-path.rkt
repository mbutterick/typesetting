#lang debug racket/base
(require racket/promise
         racket/string
         "unsafe/fontconfig.rkt")
(provide (all-defined-out))

(define fontsets-promise
  (delay
    (define font-dirs
      (case (system-type 'os)
        [(macosx)
         ;; https://support.apple.com/en-us/HT201722
         (list "/System/Library/Fonts"
               "/Library/Fonts"
               (expand-user-path "~/Library/Fonts"))]
        ;; https://wiki.ubuntu.com/Fonts#Manually
        [(unix) (list "/usr/share/fonts"
                      "/usr/local/share/fonts"
                      (expand-user-path "~/.fonts"))]
        [else ;; windows
         ;; https://support.microsoft.com/en-us/help/314960/how-to-install-or-remove-a-font-in-windows
         ;; on my windows 10 VM, the 'sys-dir is C:\\Windows\system32
         ;; though I'm suspicious that it's always like this
         (list (build-path (find-system-path 'sys-dir) 'up "fonts"))]))
    (define (path->fontset path-or-path-string)
      (define bytepath (path->bytes (if (string? path-or-path-string)
                                        (string->path path-or-path-string)
                                        path-or-path-string)))
      ((cond
         [(fc-file-is-dir bytepath) fc-dir-scan]
         [else fc-file-scan]) bytepath))
    (map path->fontset font-dirs)))

(define (probably-successful-match? family-name result)
  ;; fontconfig does its best to find a match using fuzzy logic
  ;; so there's no official failure condition, it seems.
  ;; this test checks if the first letters (up to 6) of the family name
  ;; appear somewhere in the font filename.
  (define-values (dir name _) (split-path result))
  (define name-string (path->string name))
  (regexp-match (string-downcase (substring family-name 0 (min 6 (string-length family-name)))) (string-downcase name-string)))

;; this function follows c sample
;; https://gist.github.com/CallumDev/7c66b3f9cf7a876ef75f

(define (family->path family-name #:bold [bold? #f] #:italic [italic? #f])
  ;; create a configuration & invoke it
  (fc-config-set-current (fc-config-create))
  (define fontsets (force fontsets-promise))
  (cond
    [(ormap values fontsets)
     ;; query pattern syntax
     ;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html#AEN36
     (define query-pattern
       (fc-name-parse (string->bytes/utf-8 (format "~a:weight=~a:slant=~a" family-name (if bold? 200 80) (if italic? 100 0)))))
     (fc-config-substitute query-pattern 'FcMatchPattern)
     ;; https://www.freedesktop.org/software/fontconfig/fontconfig-devel/fcdefaultsubstitute.html
     ;; Supplies default values for underspecified font patterns
     (fc-default-substitute query-pattern)
     (define result-pattern (fc-font-set-match fontsets query-pattern))
     (define result
       (and result-pattern (bytes->path (fc-pattern-get-string result-pattern #"file" 0))))
     (cond
       [(and result (probably-successful-match? family-name result)) result]
       [else #false])]
    [else #false]))
