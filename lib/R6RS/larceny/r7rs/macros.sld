;;; Macros for (scheme base) and other R7RS libraries.

(define-library (larceny r7rs macros)

  (export

   define-record-type
   define-values
   parameterize
   syntax-error)

  (import (rnrs base)
          (rnrs mutable-pairs) ; needed by define-values
          (for (rename (only (core syntax-rules)
                             r7rs:syntax-error)
                       (r7rs:syntax-error syntax-error))
               expand run)
          (rename (only (rnrs records syntactic original)
                        define-record-type)
                  (define-record-type r6rs-define-record-type))
          (rename (only (err5rs records syntactic original)
                        define-record-type)
                  (define-record-type r7rs-define-record-type))
          (primitives parameter?))


  (include "macros.body.scm")

  )
