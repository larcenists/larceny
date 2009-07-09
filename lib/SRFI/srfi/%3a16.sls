; SRFI 16: CASE-LAMBDA
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-16/srfi-16.html> for the full document.

(library (srfi :16 case-lambda)
  (export case-lambda)
  (import (only (rnrs control) case-lambda)))

(library (srfi :16)
  (export case-lambda)
  (import (srfi :16 case-lambda)))

; eof