(define-library (r6rs lists)
  (export
   find
   for-all exists
   filter partition
   fold-left fold-right
   remp remove remv remq
   memp member memv memq
   assp assoc assv assq
   cons*
   )
  (import
   (scheme base)
   (scheme case-lambda))
  (include "lists.body.scm"))
