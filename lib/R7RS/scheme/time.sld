;;; FIXME: this test of include-library-declarations doesn't show up
;;; in (features), but it works.
;;;
;;; (define-library (scheme time)
;;; 
;;;   (include-library-declarations "time.decls.scm"))

(define-library (scheme time)

  (export current-second current-jiffy jiffies-per-second)

  (import (scheme base)
          (only (larceny r7rs primitives)
                current-second)
          (only (larceny r7rs primitives lowlevel)
                current-seconds
                current-utc-time
                jiffies-per-second))

  (include "time.body.scm"))

