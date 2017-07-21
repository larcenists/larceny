(library (rnrs programs (6))
  (export command-line exit)
  (import
   (core primitives)
   (except (rnrs base)
           let-syntax
           letrec-syntax)
   (primitives
    command-line-arguments               ; [Larceny]
    exit))

  ; [Larceny]

  (define (command-line)
    (cons "larceny" (vector->list (command-line-arguments))))
  )

