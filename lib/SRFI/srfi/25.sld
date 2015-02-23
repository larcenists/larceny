;;; SRFI-25: arrays
;;; 2004-01-11 / lth
;;;
;;; $Id$
;;;

(define-library (srfi 25 multi-dimensional-arrays)

  (export array? make-array shape array array-rank
          array-start array-end array-ref array-set! share-array)

  (import (srfi :25 multi-dimensional-arrays)))

(define-library (srfi 25)

  (export array? make-array shape array array-rank
          array-start array-end array-ref array-set! share-array)

  (import (srfi 25 multi-dimensional-arrays)))

; eof
