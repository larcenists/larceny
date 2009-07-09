; Test suite for SRFI 98
;
; $Id$

(cond-expand (srfi-98))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (string? (get-environment-variable "PATH"))
    (fail 'PATH))

(or (eq? #f (get-environment-variable "Unlikely To Be Any Such Thing"))
    (fail 'Unlikely))

(or (let ((alist (get-environment-variables)))
      (and (list? alist)
           (for-all pair? alist)
           (assoc "PATH" alist)))
    (fail 'get-environment-variables))

(writeln "Done.")

; eof
