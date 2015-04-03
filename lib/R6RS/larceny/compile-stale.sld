;;; 

(define-library (larceny compile-stale)
  (export compile-stale)
  (import (scheme base)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme file)
          (rnrs hashtables)
          (only (srfi 1 lists) lset-union)
          (primitives larceny:available-source-libraries
                      larceny:absolute-path
                      larceny:libname-without-version
                      *library-keywords*
                      current-directory
                      require r5rs:require current-require-path
                      compile-r6rs-file compile-stale-libraries))

  (include "compile-stale.body.scm"))
