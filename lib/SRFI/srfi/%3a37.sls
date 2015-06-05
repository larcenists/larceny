;;; SRFI 37: args-fold: a program argument processor
;;;
;;; $Id$

(library (srfi :37)

  (export
   option
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor
   args-fold)

  (import (srfi 37)))

; eof
