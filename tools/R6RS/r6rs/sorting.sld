(define-library (r6rs sorting)
  (export list-sort vector-sort vector-sort!)
  (import (scheme base))
  (include "sorting.body.scm"))
