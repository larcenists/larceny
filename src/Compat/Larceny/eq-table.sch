; Tables keyed by object identity (eq?), for the compiler, which is
; also part of the bootstrap heap (as the interpreter's macro expander).
; Other hosts define these in their compat.sch.
;
;   (compat:make-eq-table)
;   (compat:eq-table-ref table key default)
;   (compat:eq-table-set! table key value)

(define (compat:make-eq-table) (make-eq-hashtable))
(define (compat:eq-table-ref table key default) (hashtable-ref table key default))
(define (compat:eq-table-set! table key value) (hashtable-set! table key value))

; eof
