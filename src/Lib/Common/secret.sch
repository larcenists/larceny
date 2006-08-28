; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- secret names for the macro expanders.
;
; These names _must_ be synchronized in exactly five places (gag!):
;
; - in Compiler/*.imp.sch, in the definition of the primop table and
;                          in the definition of name:LIST
; - in Eval/macro-expand.sch, at the end.
; - in Compiler/usual.sch, in the definition of quasiquote
; - in Lib/<sys>/toplevel.sch, in the definition of the top-level environments
; - in this file.
; 
; Very bad things happen if the names are not synchronized.
;
; Note also that for reasons having to do with potentially evil interactions
; with Twobit, magic names should contain no underscores as long as their
; prefix is the same as the compiler's illegal-identifier prefix (namely, ".").

($$trace "secret")

(define .make-promise make-promise)	; In Lib/control.sch
(define .append append)			; In Lib/list.sch
(define .list list)			; In Lib/list.sch
(define .list->vector list->vector)	; In Lib/vector.sch
(define .cons cons)			; In Lib/machine/primops.sch
(define .car car)                       ; In Lib/machine/primops.sch
(define .cdr cdr)                       ; In Lib/machine/primops.sch
; .make-cell, .cell-ref, and .cell-set! are already defined in primops.sch.

; eof
