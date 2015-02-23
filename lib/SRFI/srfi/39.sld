;;; SRFI-39: parameter objects
;;;
;;; $Id$
;;;

(define-library (srfi 39 parameters)

  (export make-parameter parameterize)

  (import (srfi :39 parameters)))

(define-library (srfi 39)
  (export make-parameter parameterize)
  (import (srfi 39 parameters)))

; eof
