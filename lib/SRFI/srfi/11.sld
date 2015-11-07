; SRFI 11: LET-VALUES
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-11/srfi-11.html> for the full document.

(define-library (srfi 11 let-values)
  (export let-values let*-values)
  (import (rnrs base)))

(define-library (srfi 11)
  (export let-values let*-values)
  (import (srfi 11 let-values)))

; eof