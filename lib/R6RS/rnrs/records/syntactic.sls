;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (rnrs records syntactic)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs records syntactic (6))

  (export define-record-type
          record-type-descriptor record-constructor-descriptor)

  (import (only (rnrs records syntactic original)
                record-type-descriptor
                record-constructor-descriptor)
          (only (larceny r7rs macros)
                define-record-type)))
