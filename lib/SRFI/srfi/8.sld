;;; SRFI 8: RECEIVE: Binding to multiple values
;;; Reference implementation
;;;
;;; $Id$
;;;

(define-library (srfi 8 receive)

  (export receive)

  (import (srfi :8 receive)))


(define-library (srfi 8)
  (export receive)
  (import (srfi 8 receive)))

; eof
