; Coroutines.
; 2000-05-25 / lth
;
; See Christopher Haynes, Daniel Friedman, Mitchell Wand,
;     "Continuations and Coroutines".
;     ACM Conference on Lisp and Functional Programming, 1984.
;
; To create a coroutine, call MAKE-COROUTINE on a procedure that accepts
; two values: a RESUME function and a VALUE.
;
; A coroutine is a procedure of one argument.  To transfer control to it,
; just call it with a VALUE.  To suspend the current coroutine and transfer
; control to another, use the RESUME function to transfer control.
;
; It is an error for a coroutine to return.
;
; FIXME
;   - adapt to multiple values

(define (make-coroutine f)
  (call-with-current-continuation
   (lambda (maker)
     (let ((LCS '*))                    ; "Local Control State"
       (let ((resume
              (lambda (dest val)
                (call-with-current-continuation
                 (lambda (k)
                   (set! LCS k)
                   (dest val))))))
         (f resume
            (resume maker
                    (lambda (v) (LCS v))))
         (error "Coroutine fell off the end."))))))

; eof
