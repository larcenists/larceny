(define-library (scheme repl)

  (export interaction-environment)

  (import (rnrs base)
          (larceny r7rs primitives lowlevel))

  (include "repl.body.scm"))
