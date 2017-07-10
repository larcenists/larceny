;;; Larceny's compilation manager, such as it is.

(define-library (larceny compile-stale)
  (export compile-stale
          compile-stale-cautiously
          compile-stale-regardless
          compile-stale-recklessly)
  (import (scheme base)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme file)
          (scheme list)
          (only (scheme text) textual-prefix?)
          (srfi 126)  ; hashtables
          (only (larceny compiler)
                compile-file
                compile-stale-libraries)

          (primitives ;; defined in lib/R7RS/r7rs-cond-expander.sch

                      larceny:available-source-libraries
                      larceny:library->entry
                      larceny:make-library-entry
                      larceny:library-entry-name
                      larceny:library-entry-exports
                      larceny:library-entry-imports
                      larceny:library-entry-filename
                      larceny:library-entry-multiple?

                      ;; defined in lib/R6RS/r6rsmode.sch

                      larceny:absolute-path
                      larceny:libname-without-version
                      *library-keywords*
                      generate-fasl-name
                      compile-r6rs-file
                      compile-stale-libraries
                      r5rs:require

                      ;; defined in Larceny's R5RS heap

                      require
                      current-require-path
                      current-directory
                      file-newer?
                      pretty-print))

  (begin (define (debugging?) #f))

  (include "compile-stale.body.scm"))
