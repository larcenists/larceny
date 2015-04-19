; SRFI 9
; Records
;
; $Id$
;

(define-library (srfi 9 records)
  (export define-record-type)
  (import (srfi :9 records)))

(define-library (srfi 9)
  (export define-record-type)
  (import (srfi 9 records)))

; eof
