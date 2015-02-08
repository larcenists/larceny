;;; R7RS procedures imported from Larceny's R5RS substrate.

(define-library (larceny r7rs primitives)

  (export

   bytevector
   bytevector-append
   char-ready?
   current-second
   display
   emergency-exit
   error-object-irritants
   error-object-message
   error-object?
   exact-integer?
   exit
   file-error?
   floor-quotient
   floor-remainder
   floor/
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
   read-error?
   read-line
   square
   truncate-quotient
   truncate-remainder
   truncate/
   write
   write-shared
   write-simple
   write-string

   input-port-open?
   output-port-open?
   peek-u8
   read-bytevector
   read-bytevector!
   read-string
   read-u8
   string->vector
   string-copy!
   string-fill!
   string-map
   syntax-error
   u8-ready?
   vector->string
   vector-append
   vector-copy
   vector-copy!
   write-bytevector
   write-u8

   )

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
    write-string

    bytevector
    bytevector-append
    error-object-irritants
    error-object-message
    error-object?
    exact-integer?
    file-error?
    floor-quotient
    floor-remainder
    floor/
    input-port-open?
    output-port-open?
    peek-u8
    read-bytevector
    read-bytevector!
    read-error?
    read-string
    read-u8
    square
    string->vector
    string-copy!
    string-fill!
    string-map
    syntax-error
    truncate-quotient
    truncate-remainder
    truncate/
    u8-ready?
    vector->string
    vector-append
    vector-copy
    vector-copy!
    write-bytevector
    write-u8

    )))

#|
    bytevector
    bytevector-append

    error-object-irritants
    error-object-message
    error-object?
    file-error?
    read-error?

    exact-integer?
    floor-quotient
    floor-remainder
    floor/
    truncate-quotient
    truncate-remainder
    truncate/
    square

    input-port-open?
    output-port-open?
    peek-u8
    u8-ready?

    read-bytevector
    read-bytevector!
    read-string
    read-u8
    write-bytevector
    write-u8

    string->vector
    string-copy!
    string-fill!
    string-map

    syntax-error

    vector->string
    vector-append
    vector-copy
    vector-copy!
|#
