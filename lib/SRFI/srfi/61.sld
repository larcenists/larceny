; SRFI 61: a more general cond clause
;
; $Id$
;
; Conflicts with (rnrs base): cond
;

(define-library (srfi 61 cond)

  (export cond)

  (import (srfi :61 cond)))


(define-library (srfi 61)

  (export cond)

  (import (srfi 61 cond)))

; eof
