;;; The three specific environments specified in R7RS 6.12 go into
;;; the (scheme r5rs) library.

(define-library (scheme eval)

  (export environment eval)

  (import (rnrs eval)))
