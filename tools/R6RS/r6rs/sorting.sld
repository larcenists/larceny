(define-library (r6rs sorting)
  (export list-sort vector-sort vector-sort!)

  (cond-expand

   ((and (library (rnrs sorting))
         (not (library (r6rs no-rnrs))))
    (import (rnrs sorting)))

   (else
    (import (scheme base))
    (include "sorting.body.scm"))))
