(define-library (srfi 123)
  (export
   ref ref* ~ register-getter-with-setter!
   $bracket-apply$
   set! setter getter-with-setter)
  (import
   (except (scheme base) set! define-record-type)
   (scheme case-lambda)
   (srfi 1)
   (srfi 17)
   (srfi 31))
  (cond-expand
   ((library (r6rs hashtables))
    (import (r6rs hashtables)))
   ((library (rnrs hashtables))
    (import (rnrs hashtables))))
  (cond-expand
   ;; Favor SRFI-99.
   ((library (srfi 99))
    (import (srfi 99)))
   ;; We assume that if there's the inspection library, there's also the
   ;; syntactic and procedural libraries.
   ((library (rnrs records inspection))
    (import (rnrs records syntactic))
    (import (rnrs records procedural))
    (import (rnrs records inspection)))
   ((library (r6rs records inspection))
    (import (r6rs records syntactic))
    (import (r6rs records procedural))
    (import (r6rs records inspection)))
   (else
    (import (rename (only (scheme base) define-record-type)
                    (define-record-type %define-record-type)))
    (export define-record-type)))
  (cond-expand
   ((library (srfi 4))
    (import (srfi 4)))
   (else))
  (cond-expand
   ((library (srfi 111))
    (import (srfi 111)))
   (else))
  (include "123.body.scm"))
