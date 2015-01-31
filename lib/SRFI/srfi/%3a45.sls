;;; SRFI 45: Primitives for expressing iterative lazy algorithms
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs r5rs): delay, force
;;;

(library (srfi :45 lazy)

  (export delay lazy force eager)

  (import (only (larceny r7rs promises)
                delay lazy force eager)))

; eof
