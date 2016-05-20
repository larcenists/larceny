;;; Other procedures imported from Larceny's R5RS substrate
;;; so they can be used to define R7RS procedures.

(define-library (larceny r7rs primitives lowlevel)

  (export

   current-seconds
   (rename larceny:current-utc-time current-utc-time)
   ex:interaction-environment
   jiffies-per-second
   larceny:features
   larceny:use-r7rs-semantics!
   r7rs:bytevector-copy!
   )

  (import

   (larceny r7rs primitives patches) ; FIXME

   (primitives

    current-seconds
    ex:interaction-environment
    larceny:features
    larceny:use-r7rs-semantics!
    r7rs:bytevector-copy!)))
