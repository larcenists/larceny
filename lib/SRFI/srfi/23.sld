; SRFI 16: ERROR
;
; $Id$
;
; Conflicts with (rnrs base): error
;
; See <http://srfi.schemers.org/srfi-23/srfi-23.html> for the full document.

(define-library (srfi 23 error)
  (export error)
  (import (only (scheme base) error)))

(define-library (srfi 23)
  (export error)
  (import (srfi 23 error)))

; eof
