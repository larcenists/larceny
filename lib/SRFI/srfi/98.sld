;;; SRFI 98
;;; An interface to access environment variables.
;;;
;;; $Id$

(define-library (srfi 98 os-environment-variables)

  (export get-environment-variable get-environment-variables)

  (import (only (scheme process-context)
                get-environment-variable
                get-environment-variables)))

(define-library (srfi 98)

  (export get-environment-variable get-environment-variables)

  (import (srfi 98 os-environment-variables)))

; eof
