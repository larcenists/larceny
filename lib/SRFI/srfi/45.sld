;;; SRFI 45: Primitives for expressing iterative lazy algorithms
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs r5rs): delay, force
;;;

(define-library (srfi 45 lazy)

  (export delay lazy force eager)

  (import (srfi :45 lazy)))

; eof
