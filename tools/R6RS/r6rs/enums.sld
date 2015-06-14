(define-library (r6rs enums)
  (export
   make-enumeration
   enum-set-universe enum-set-indexer enum-set-constructor enum-set->list
   enum-set-member? enum-set-subset? enum-set=?
   enum-set-union enum-set-intersection enum-set-difference
   enum-set-complement
   enum-set-projection
   define-enumeration
   )

  (cond-expand

   ((and (library (rnrs enums))
         (not (library (r6rs no-rnrs))))
    (import (rnrs enums)))

   (else
    (import
     (scheme base)
     (r6rs lists)
     (r6rs sorting))
    (include "enums.body.scm"))))
