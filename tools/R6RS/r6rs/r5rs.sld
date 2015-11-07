(define-library (r6rs r5rs)
  (export
   exact->inexact inexact->exact
   quotient remainder modulo
   delay force
   null-environment scheme-report-environment)
  (import (scheme r5rs)))
