;;; FIXME: this test of include-library-declarations doesn't show up
;;; in (features), but it works.

(define-library (scheme time)

  (include-library-declarations "time.decls.scm"))
