(define-library (scheme complex)

  (export

   angle
   imag-part
   magnitude
   make-polar
   make-rectangular
   real-part
   )

  (import
   (only (rnrs base)
         angle
         imag-part
         magnitude
         make-polar
         make-rectangular
         real-part)))
