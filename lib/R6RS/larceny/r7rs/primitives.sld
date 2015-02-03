;;; R7RS procedures imported from Larceny's R5RS substrate.

(define-library (larceny r7rs primitives)

  (export

   char-ready?
   current-second
   display
   emergency-exit
   exit
   get-environment-variables
   getenv
   get-output-bytevector
   list-copy
   list-set!
   make-list
   make-parameter
   open-binary-input-file
   open-binary-output-file
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
    current-second
    display
    emergency-exit
    exit
    get-environment-variables
    getenv
    get-output-bytevector
    list-copy
    list-set!
    make-list
    make-parameter
    open-binary-input-file
    open-binary-output-file
    open-input-bytevector
    open-output-bytevector
    read-line
    write
    write-shared
    write-simple
    write-string)))

