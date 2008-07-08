(library (larceny compiler)
  (export load require current-require-path
          compile-file compile-library compile-stale-libraries)
  (import (rnrs base)
          (err5rs load)
          (primitives require current-require-path
                      compile-r6rs-file compile-stale-libraries))

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
            (cons src rest)))))
  )
