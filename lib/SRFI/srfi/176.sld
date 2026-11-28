(define-library (srfi 176)

  (export version-alist)

  (import (scheme base)
          (only (scheme list) filter)
          (primitives system-features))

  (include "176.body.scm"))
