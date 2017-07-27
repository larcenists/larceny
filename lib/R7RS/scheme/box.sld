;;; (scheme box)
;;;
;;; R7RS Red Edition

(define-library (scheme box)
  (export box box? unbox set-box!)
  (import (srfi 111)))
