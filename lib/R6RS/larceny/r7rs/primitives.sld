;;; R7RS procedures imported from Larceny's R5RS substrate.

(define-library (larceny r7rs primitives)

  (export

   char-ready?
   display
   get-output-bytevector
   list-copy
   list-set!
   make-list
   make-parameter
   open-input-bytevector
   open-output-bytevector
   read-line
   write
   write-shared
   write-simple
   write-string)

  (import

   (primitives

    char-ready?
    display
    get-output-bytevector
    list-copy
    list-set!
    make-list
    make-parameter
    open-input-bytevector
    open-output-bytevector
    read-line
    write
    write-shared
    write-simple
    write-string)))

;;; Other procedures imported from Larceny's R5RS substrate
;;; so they can be used to define R7RS procedures.

(define-library (larceny r7rs primitives lowlevel)

  (export

   getenv
   interaction-environment
   larceny:features
   )

  (import

   (primitives

    getenv
    interaction-environment
    larceny:features)))
