; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Given a Petit Larceny compiled code-less toplevel thunk, and an 
; ID/number pair that denote its code vector, fetch the code vector 
; from system tables and patch the procedure, returning the procedure.

; WARNING: no error checking.  
;
; A simple and fast error check would be a serial number shared by 
; the executable and the .LOAD files, passed as an argument to this 
; procedure.

(define (.petit-patch-procedure id number proc)
  (procedure-set! proc 0 (syscall syscall:segment-code-address id number))
  proc)

; eof
