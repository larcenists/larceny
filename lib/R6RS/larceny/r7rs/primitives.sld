;;; R7RS procedures imported from Larceny's R5RS substrate.

(define-library (larceny r7rs primitives)

  (export

   char-ready?
   get-output-bytevector
   list-copy
   list-set!
   make-list
   make-parameter
   open-input-bytevector
   open-output-bytevector
   read-line
   write-string
   )

  (import

   (primitives

    char-ready?
    get-output-bytevector
    list-copy
    list-set!
    make-list
    make-parameter
    open-input-bytevector
    open-output-bytevector
    read-line
    write-string)))
