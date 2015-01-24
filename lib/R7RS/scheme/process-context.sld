(define-library (scheme process-context)

  (export

   command-line
   emergency-exit
   exit
   get-environment-variable
   get-environment-variables
   )

  (import (rnrs base)
          (rnrs programs)
          (larceny r7rs primitives lowlevel))

  (include "process-context.body.scm"))
