;;; SRFI 66: Octet Vectors
;;;
;;; $Id$

(define-library (srfi 66 octet-vectors)

  (export u8vector? make-u8vector u8vector u8vector->list list->u8vector
          u8vector-length u8vector-ref u8vector-set!
          u8vector=? u8vector-compare
          u8vector-copy! u8vector-copy)

  (import (srfi :66 octet-vectors)))


(define-library (srfi 66)

  (export u8vector? make-u8vector u8vector u8vector->list list->u8vector
          u8vector-length u8vector-ref u8vector-set!
          u8vector=? u8vector-compare
          u8vector-copy! u8vector-copy)

  (import (srfi 66 octet-vectors)))

; eof
