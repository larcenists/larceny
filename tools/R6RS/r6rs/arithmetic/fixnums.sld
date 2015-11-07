(define-library (r6rs arithmetic fixnums)

  (export

   fixnum?
   fixnum-width least-fixnum greatest-fixnum
   fx=? fx>? fx<? fx>=? fx<=?
   fxzero? fxpositive? fxnegative? fxodd? fxeven?
   fxmax fxmin
   fx+ fx*
   fx-
   fxdiv-and-mod fxdiv fxmod fxdiv0-and-mod0 fxdiv0 fxmod0
   fx+/carry fx-/carry fx*/carry
   fxnot fxand fxior fxxor fxif
   fxbit-count fxlength fxfirst-bit-set fxbit-set?
   fxcopy-bit fxbit-field fxcopy-bit-field
   fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right
   fxrotate-bit-field fxreverse-bit-field
   )

  (import
   (scheme base)
   (scheme case-lambda)
   (except (r6rs base) error assert))

  ;; 24 is the minimum fixnum width an implementation must support

  (cond-expand

   ((and (library (rnrs arithmetic fixnums))
         (not (library (r6rs no-rnrs))))
    (import (rnrs arithmetic fixnums)))

   ((and sagittarius x86_64)
    (begin (define W 62))
    (include "fixnums.body.scm"))

   ((and larceny ilp32)
    (begin (define W 30))
    (include "fixnums.body.scm"))

   (else
    (begin (define W 24))
    (include "fixnums.body.scm")))

  )

; eof

