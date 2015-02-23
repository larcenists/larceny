; SRFI-2
; $Id$

(define-library (srfi 2 and-let*)

  (export and-let*)

  (import (srfi :2 and-let*)))

(define-library (srfi 2)
  (export and-let*)
  (import (srfi 2 and-let*)))

; eof
