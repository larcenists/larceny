; Lib/secret.sch
; Larceny -- secret names for the macro expanders.
;
; $Id$
;
; These names _must_ be synchronized in exactly five places:
;
; - in Compiler/*.imp.sch, in the definition of the primop table and
;                          in the definition of name:LIST
; - in Eval/macro-expand.sch, in the definition of quasiquote
; - in Compiler/pass1.aux.sch, in the definition of quasiquote
; - in Eval/toplevel.sch, in the definition of the top-level environments
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
(define .cons cons)			; In Lib/primops.sch

; eof
