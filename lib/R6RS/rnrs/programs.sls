(library (rnrs programs (6))
  (export command-line exit)
  (import
   (core primitives)
   (rnrs base)
   (primitives
    command-line-arguments               ; [Larceny]
    exit))

  ; [Larceny]

  (define (command-line)
    (cons "larceny" (vector->list (command-line-arguments))))
  )

