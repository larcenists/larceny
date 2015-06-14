;;; SRFI 37: args-fold: a program argument processor
;;;
;;; $Id$

(define-library (srfi 37)

  (export
   option
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor
   args-fold)

  (import (scheme base)
          (srfi 11))

  (include "37.body.scm"))

; eof
