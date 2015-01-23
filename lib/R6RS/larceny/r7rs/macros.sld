;;; Macros for (scheme base) and other R7RS libraries.

(define-library (larceny r7rs macros)

  (export parameterize define-values)

  (import (rnrs base))

  (include "macros.body.scm")

  )
