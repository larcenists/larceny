; Monitors -- synchronized LAMBDA expressions.
; 2004-01-18 / lth
;
; Code inside a monitor is preemptable, but only one thread (active or
; not) can be inside the monitor at any one time.

(require 'mutex)

(define-syntax sync-lambda 
  (syntax-rules ()
    ((sync-lambda FORMALS BODY ...)
     (let ((m (make-mutex)))
       (lambda FORMALS
         (dynamic-wind 
          (lambda () (mutex-acquire m))
          (lambda ()
            BODY ...)
          (lambda () (mutex-release m))))))))

