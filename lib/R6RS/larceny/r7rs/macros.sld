;;; Macros for (scheme base) and other R7RS libraries.

(define-library (larceny r7rs macros)

  (export

   define-record-type
   define-values
   parameterize)

  (import (rnrs base)
          (rename (only (rnrs records syntactic original)
                        define-record-type)
                  (define-record-type r6rs-define-record-type))
          (rename (only (err5rs records syntactic original)
                        define-record-type)
                  (define-record-type r7rs-define-record-type)))


  (include "macros.body.scm")

  )
