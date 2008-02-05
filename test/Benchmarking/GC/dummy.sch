; Dummy benchmark (for testing)
;
; $Id: dummy.sch,v 1.2 1999/07/12 18:03:37 lth Exp $

(define (dummy-benchmark . args)
  (run-benchmark "dummy"
                 1
                 (lambda ()
                   (collect)
                   (display "This is the dummy benchmark!")
                   (newline)
                   (display "My arguments are: ")
                   (display args)
                   (newline)
                   args)
                 (lambda (result)
                   (equal? result args))))

; eof
