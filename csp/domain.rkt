#lang racket/base
(require racket/class racket/list "helper.rkt")
(provide (all-defined-out))

;; Class used to control possible values for variables
;; When list or tuples are used as domains, they are automatically
;; converted to an instance of that class.
(define domain%  
  (class* object% (printable<%> (make-proc<%> get-values))
    (super-new)
    (init-field set)
    (field [_list set][_hidden null][_states null])
    
    (define (repr) (format "<domain% ~v>" _list))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (reset-state)
      ;; Reset to the original domain state, including all possible values
      (py-extend! _list _hidden)
      (set! _hidden null)
      (set! _states null))
    
    (define/public (push-state)
      ;; Save current domain state
      ;; Variables hidden after that call are restored when that state
      ;;  is popped from the stack.
      (py-append! _states (length _list)))
    
    (define/public (pop-state)
      ;; Restore domain state from the top of the stack
      
      ;; Variables hidden since the last popped state are then available
      ;; again.
      (define diff (- (py-pop! _states) (length _list)))
      (when (not (= 0 diff))
        (py-extend! _list (take-right _hidden diff))
        (set! _hidden (take _hidden (- (length _hidden) diff)))))
    
    (define/public (hide-value value)
      ;; Hide the given value from the domain
      
      ;; After that call the given value won't be seen as a possible value
      ;; on that domain anymore. The hidden value will be restored when the
      ;; previous saved state is popped.
      (set! _list (remove value _list))
      (py-append! _hidden value))
    
    (define/public (get-values)
      _list)
    
    (define/public (domain-pop!)
      (py-pop! _list))
    
    (define/public (copy)
      (define copied-domain (new domain% [set _list]))
      (set-field! _hidden copied-domain _hidden)
      (set-field! _states copied-domain _states)
      copied-domain)))

(define domain%? (is-a?/c domain%))

