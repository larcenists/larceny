; SRFI 17: Generalized SET
;
; $Id$
;
; Conflicts with (rnrs base): set!
;
; See <http://srfi.schemers.org/srfi-17/srfi-17.html> for the full document.

; SRFI 17 reference implementation for Twobit.
;
; Use the LET* syntax scope extension in Twobit to let this SET! macro
; reference the old definition of SET! in the second clause.

(define-library (srfi 17 generalized-set!)

  (export set! setter getter-with-setter)

  (import (srfi :17 generalized-set!)))


(define-library (srfi 17)
  (export set! setter getter-with-setter)
  (import (srfi 17 generalized-set!)))  

; eof
