(define-library (scheme lazy)

  (export delay delay-force force make-promise promise?)

  (import (only (larceny r7rs promises)
                delay delay-force force make-promise promise?)))
