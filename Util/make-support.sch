; Util/make-support.sch
; Larceny -- make utility
;
; $Id: make-support.sch,v 1.1.1.1 1998/11/19 21:51:54 lth Exp $
;
; Support code for the make utility when loaded separately from nbuild.

(define (call-with-error-control thunk1 thunk2) 
  (let ((eh (error-handler)))
    (error-handler (lambda args
		     (error-handler eh)
		     (thunk2)
		     (apply eh args)))
    (thunk1)
    (error-handler eh)))

; eof
