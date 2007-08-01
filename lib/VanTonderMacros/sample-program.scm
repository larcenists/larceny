;;;
;;; Separate compilation example:
;;;
;;; See macros-test.scm for compilation script. 
;;;


;; Start of program

(import (rnrs) (party))

(define p (make-party))
(pop! p)                      ;; displays "Boom! 108"
(push! p (push (make 5 5) 1))
(pop! p)                      ;; displays "Boom! 24"