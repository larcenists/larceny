; -*- scheme -*-
; $Id: syshooks.sch,v 1.1.1.1 1998/11/19 21:52:13 lth Exp $
;
; Larceny run-time system -- interface to system functions
;
; Simple hack that makes low-level system functions available interactively
; in a controlled manner.

($$trace "syshooks")

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
    ((sys$get-pending-asynch-signal) sys$get-pending-asynch-signal)
    ((io/read-char) io/read-char)
    ((syscall) syscall)
    ((iflush) sys$codevector-iflush)
    (else ???)))

; eof
