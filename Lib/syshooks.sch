; -*- scheme -*-
; $Id: syshooks.sch,v 1.2 1997/05/31 01:50:28 lth Exp lth $
;
; Larceny run-time system -- interface to system functions
;
; Simple hack that makes low-level system functions available interactively
; in a controlled manner.

(define (system-function name)
  (case name
    ((sys$get-resource-usage) sys$get-resource-usage)
    ((sys$close-file) sys$close-file)
    ((sys$delete-file) sys$delete-file)
    ((sys$read-file) sys$read-file)
    ((sys$write-file) sys$write-file)
    ((sys$write-file4) sys$write-file4)
    ((sys$rename-file) sys$rename-file)
    ((sys$file-modification-time) sys$file-modification-time)
    ((sys$file-exists?) sys$file-exists?)
    ((sys$char-ready?) sys$char-ready?)
    ((sys$gc) sys$gc)
    ((sys$codevector-iflush) sys$codevector-iflush)
    ((sys$tracectl) sys$tracectl)
    ((sys$trace) sys$trace)
    ((sys$gcctl) sys$gcctl)
    (else ???)))

; eof
