;;; Parsing command-line options.
;;;
;;; FIXME: This library was created for use by the compile-stale and
;;; compile-larceny scripts, but may become more generally useful
;;; over time.

(define-library (larceny parse-options)
  (export larceny:parse-options)
  (import (scheme base)
          (srfi 126))
  (include "parse-options.body.scm"))
