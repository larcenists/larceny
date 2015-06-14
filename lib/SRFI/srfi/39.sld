;;; SRFI-39: parameter objects
;;;
;;; $Id$
;;;

(define-library (srfi 39 parameters)

  (export make-parameter parameterize)

  (import (only (scheme base) make-parameter parameterize)))

(define-library (srfi 39)
  (export make-parameter parameterize)
  (import (srfi 39 parameters)))

; eof
