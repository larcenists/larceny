;;; SRFI 51: Handling rest list.
;;;
;;; $Id$

(define-library (srfi 51 rest-values)

  (export rest-values
          arg-and arg-ands
          err-and err-ands
          arg-or arg-ors
          err-or err-ors)

  (import (srfi :51 rest-values)))


(define-library (srfi 51)

  (export rest-values
          arg-and arg-ands
          err-and err-ands
          arg-or arg-ors
          err-or err-ors)

  (import (srfi 51 rest-values)))

; eof
