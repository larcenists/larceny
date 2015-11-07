;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, syntactic layer.
;
; To simplify interoperability between R7RS and R6RS, this
; implementation of define-record-type defers to Larceny's
; implementation of R7RS define-record-type, which supports
; all four of these specifications of define-record-type:
;
;     SRFI 9
;     ERR5RS
;     R6RS      (deprecated)
;     R7RS
;
; See (err5rs records syntactic original), which is imported by
; (larceny r7rs macros), which is imported here.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records syntactic)

  (export define-record-type)

  (import (only (larceny r7rs macros) define-record-type)))
