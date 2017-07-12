;;; Larceny's compilation manager.

(define-library (larceny compile-stale)
  (export compile-file
          compile-library
          compile-stale
          compile-stale-cautiously
          compile-stale-regardless
          compile-stale-recklessly
          compile-stale-libraries)
  (import (scheme base)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme file)
          (scheme list)
          (only (scheme text) textual-prefix?)
          (only (rnrs base) assertion-violation)
          (srfi 126)  ; hashtables

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

  (begin

   (define (debugging?) #f)

   (define (compile-file src . rest)
     (compile-file-shared src rest #f))

   (define (compile-library src . rest)
     (compile-file-shared src rest #t))

   (define (compile-file-shared src rest libraries-only?)
     (cond ((null? rest)
            (compile-r6rs-file src #f libraries-only?))
           ((and (string? (car rest)) (null? (cdr rest)))
            (compile-r6rs-file src (car rest) libraries-only?))
           (else
            (assertion-violation
             (if libraries-only? 'compile-library 'compile-file)
             "too many arguments"
             (cons src rest))))))

  (include "compile-stale.body.scm"))
