; SRFI 9
; Records
;
; $Id$
;
; This implementation uses the ERR5RS-proposed record package to do the
; dirty work.
;
; FIXME: That adds some extensions.

(library (srfi :9 records)
  (export define-record-type)
  (import (err5rs records syntactic)))

(library (srfi :9)
  (export define-record-type)
  (import (srfi :9 records)))

; eof
