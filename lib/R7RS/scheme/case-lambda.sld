;;; If R7RS case-lambda is different from R6RS case-lambda,
;;; I really don't want to hear about it.

(define-library (scheme case-lambda)

  (export case-lambda)

  (import (rnrs control)))
