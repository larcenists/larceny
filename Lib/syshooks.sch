; -*- scheme -*-
; $Id: syshooks.sch,v 1.5 1997/09/17 15:13:23 lth Exp lth $
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
    ((sys$gcctl) sys$gcctl)
    ((sys$get-pending-asynch-signal) sys$get-pending-asynch-signal)
    ((io/read-char) io/read-char)
    ((big-normalize!) big-normalize!)
    ((big-fits-in-fix?) big-fits-in-fix?)
    ((bignum->fixnum) bignum->fixnum)
    ((bignum-length) bignum-length)
    ((bignum-ref) bignum-ref)
    ((bigdump*) bigdump*)
    ((big-subtract-digits) big-subtract-digits)
    (else ???)))

; eof
