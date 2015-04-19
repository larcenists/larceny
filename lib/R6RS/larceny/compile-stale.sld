;;; 

(define-library (larceny compile-stale)
  (export compile-stale)
  (import (scheme base)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme file)
          (rnrs hashtables)
          (primitives larceny:available-source-libraries
                      larceny:absolute-path
                      larceny:libname-without-version
                      larceny:make-library-entry
                      larceny:library-entry-name
                      larceny:library-entry-exports
                      larceny:library-entry-imports
                      larceny:library-entry-filename
                      larceny:library-entry-multiple?
                      *library-keywords*
                      current-directory
                      generate-fasl-name
                      file-newer?
                      require r5rs:require current-require-path
                      compile-r6rs-file compile-stale-libraries))

  (begin (define (debugging?) #t))

  (include "compile-stale.body.scm"))
