;;; SRFI 31: REC
;;; Reference implementation.
;;;
;;; $Id$
;;;

(define-library (srfi 31 rec)

  (export rec)

  (import (srfi :31 rec)))

(define-library (srfi 31)
  (export rec)
  (import (srfi 31 rec)))

; eof
