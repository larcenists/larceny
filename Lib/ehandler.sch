; Lib/ehandler.sch
; Larceny library -- system exception handler.
;
; $Id: ehandler.sch,v 1.2 1997/02/03 20:07:13 lth Exp $
;
; The procedure "exception-handler" takes the contents of RESULT, 
; ARGREG2, and ARGREG3, and an exception code as arguments. It 
; just calls the current error handler procedure. The default error
; handler will call back in to "system-error-handler", which takes
; the same arguments (but with the code first), and then dispatches
; on the code and the contents of the three registers to provide a 
; useful error message to the user; 'error' is then called with the
; pertinent arguments to display the error.
;
; The boolean "print-object?" controls whether the object in error
; is to be printed with the error message. This resolution may be
; a little coarse; it works for now.
;
; Implementation: it would be more reasonable to use a vector here.

($$trace "ehandler")

(define (exception-handler arg1 arg2 arg3 code)
  (if (= code $ex.timer)
      ((interrupt-handler) 'timer)
      ((error-handler) code arg1 arg2 arg3)))

; If ``print-object?'' is true then the faulting object is printed as part
; of the error message, otherwise it is not printed.

(define print-object? #t)

; The default system exception handler

(define system-error-handler
  (let ()

    (define (not-a-pair name obj)
      (if print-object?
	  (error name ": " obj " is not a pair.")
	  (error name ": not a pair.")))

    (define (not-a-num name obj)
      (if print-object?
	  (error (string-append name ": ") obj " is not a number.")
	  (error (string-append name ": not a number."))))

    (define (not-a-real name obj)
      (if print-object?
	  (error (string-append name ": ") obj " is not a real number.")
	  (error (string-append name ": not a real number."))))

    (define (not-an-int name obj)
      (if print-object?
	  (error (string-append name ": ") obj " is not an integer.")
	  (error (string-append name ": not an integer."))))

    (define (div-by-zero name obj1 obj2)
      (error (string-append name ":  division by zero: ") obj1 " " obj2))

    (define (not-a-fix name obj)
      (if print-object?
	  (error (string-append name ": ") obj " is not a fixnum.")
	  (error (string-append name ": not a fixnum."))))

    (define (num-binop name arg1 arg2)
      (cond ((not (number? arg1)) (not-a-num name arg1))
	    ((not (number? arg2)) (not-a-num name arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (num-div-binop name arg1 arg2)
      (cond ((not (number? arg1)) (not-a-num name arg1))
	    ((not (number? arg2)) (not-a-num name arg2))
	    ((zero? arg2) (div-by-zero name arg1 arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (real-binop name arg1 arg2)
      (cond ((not (real? arg1)) (not-a-real name arg1))
	    ((not (real? arg2)) (not-a-real name arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (int-binop name arg1 arg2)
      (cond ((not (integer? arg1)) (not-an-int name arg1))
	    ((not (integer? arg2)) (not-an-int name arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (int-div-binop name arg1 arg2)
      (cond ((not (integer? arg1)) (not-an-int name arg1))
	    ((not (integer? arg2)) (not-an-int name arg2))
	    ((zero? arg2) (div-by-zero name arg1 arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (fix-binop name arg1 arg2)
      (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
	    ((not (fixnum? arg2)) (not-a-fix name arg2))
	    (else (error "exception-handler: confused about " name))))

    (define (dstruct code reffer thing test? length arg1 arg2 . rest)
      (let ((name (string-append thing (if (= code reffer) "-ref:" "-set!:"))))
	(cond ((not (test? arg1))
	       (if print-object?
		   (error name " " arg1 " is not a " thing)
		   (error name " not a " thing)))
	      ((or (not (fixnum? arg2))
		   (< arg2 0)
		   (>= arg2 (length arg1)))
	       (if print-object?
		   (error name " " arg2 " is not a valid index into " thing)
		   (error name " invalid index into " thing)))
	      ((and (not (null? rest))
		    (not (= code reffer))
		    (not ((car rest) (cadr rest))))
	       (error name " " arg3 " cannot be stored in a " thing))
	      (else
	       (if (bignum? arg1)
		   (begin (display "BIG: ") (bigdump* arg1) (newline)))
	       (error "Exception-handler: confused about " 
		      name " " arg1 " " arg2)))))

    (define (charpred name arg1 arg2)
      (error name ": " (if (char? arg1) arg2 arg1) " is not a character."))

    (define (byte? x)
      (and (integer? x) (exact? x) (<= 0 x 255)))

    (define (handler code arg1 arg2 arg3)
      (cond 

       ;; Timer interrupt

       ((= code $ex.enable-interrupts)
	(cond ((not (fixnum? arg1))
	       (error "enable-interrupts: not a fixnum: " arg1))
	      ((<= arg1 0)
	       (error "enable-interrupts: not positive: " arg1))
	      (else
	       (error "enable-interrupts: confused: "
		      arg1 " " arg2 " " arg3))))

       ;; Pairs

       ((= code $ex.car)
	(not-a-pair "car" arg1))
       ((= code $ex.cdr)
	(not-a-pair "cdr" arg1))
       ((= code $ex.setcar)
	(not-a-pair "setcar" arg1))
       ((= code $ex.setcdr)
	(not-a-pair "setcdr" arg1))

       ;; Numbers

       ((= code $ex.add)
	(num-binop "+" arg1 arg2))
       ((= code $ex.sub)
	(num-binop "-" arg1 arg2))
       ((= code $ex.mul)
	(num-binop "*" arg1 arg2))
       ((= code $ex.div)
	(num-div-binop "/" arg1 arg2))
       ((= code $ex.lessp)
	(real-binop "<" arg1 arg2))
       ((= code $ex.lesseqp)
	(real-binop "<=" arg1 arg2))
       ((= code $ex.equalp)
	(num-binop "=" arg1 arg2))
       ((= code $ex.greatereqp)
	(real-binop ">=" arg1 arg2))
       ((= code $ex.greaterp)
	(real-binop ">" arg1 arg2))
       ((= code $ex.quotient)
	(int-div-binop "quotient" arg1 arg2))
       ((= code $ex.remainder)
	(int-div-binop "remainder" arg1 arg2))
       ; NOTE: modulo is no longer a primop, so this case is obsolete
       ((= code $ex.modulo)
	(int-binop "modulo" arg1 arg2))
       ((= code $ex.logior)
	(fix-binop "logior" arg1 arg2))
       ((= code $ex.logand)
	(fix-binop "logand" arg1 arg2))
       ((= code $ex.logxor)
	(fix-binop "logxor" arg1 arg2))
       ((= code $ex.lognot)
	(not-a-fix "lognot" arg1))
       ((= code $ex.lsh)
	(fix-binop "lsh" arg1 arg2))
       ((= code $ex.rsha)
	(fix-binop "rsha" arg1 arg))
       ((= code $ex.rshl)
	(fix-binop "rshl" arg1 arg2))
       ((= code $ex.e2i)
	(not-a-num "exact->inexact" arg1))
       ((= code $ex.i2e)
	(not-a-num "inexact->exact" arg1))
       ((= code $ex.exactp)
	(not-a-num "exact?" arg1))
       ((= code $ex.inexactp)
	(not-a-num "inexact?" arg1))
       ((= code $ex.round)
	(if (not (number? arg1))
	    (not-a-num "round" arg1)
	    (not-a-real "round" arg1)))
       ((= code $ex.trunc)
	(if (not (number? arg1))
	    (not-a-num "trunc" arg1)
	    (not-a-real "trunc" arg1)))
       ((= code $ex.zerop)
	(not-a-num "zero?" arg1))
       ((= code $ex.neg)
	(not-a-num "-" arg1))
       ((= code $ex.abs)
	(if (not (number? arg1))
	    (not-a-num "abs" arg1)
	    (not-a-real "abs" arg1)))
       ((= code $ex.realpart)
	(not-a-num "real-part" arg1))
       ((= code $ex.imagpart)
	(not-a-num "imag-part" arg1))

       ;; Vectors

       ((or (= code $ex.vref) (= code $ex.vset))
	(dstruct code $ex.vref "vector" vector? vector-length arg1 arg2))
       ((= code $ex.vlen)
	(error "Vector-length: " arg1 " is not a vector."))

       ;; Procedures

       ((or (= code $ex.pref) (= code $ex.pset))
	(dstruct code $ex.pref "procedure" procedure? procedure-length
		 arg1 arg2))
       ((= code $ex.plen)
	(error "Procedure-length: " arg1 " is not a procedure."))

       ;; Vector-like

       ((= code $ex.mkvl)
	(error "make-vector-like: " arg1 
	       " is not an exact nonnegative integer."))

       ((or (= code $ex.vlref) (= code $ex.vlset))
	(dstruct code $ex.vlref "vector-like" vector-like? vector-like-length
		 arg1 arg2))

       ((= code $ex.vllen)
	(error "vector-like-length: " arg1 " is not a vector-like."))

       ;; Strings

       ((or (= code $ex.sref) (= code $ex.sset))
	(dstruct code $ex.sref "string" string? string-length arg1 arg2
		 char? arg3))
       ((= code $ex.slen)
	(error "string-length: " arg1 " is not a string"))

       ;; Bytevectors

       ((or (= code $ex.bvref) (= code $ex.bvset))
	(dstruct code $ex.bvref "bytevector" bytevector? bytevector-length
		 arg1 arg2 byte? arg3))
       ((= code $ex.bvlen)
	(error "bytevector-length: " arg1 " is not a bytevector."))
      
       ((= code $ex.bvfill)
	(if (not (bytevector? arg1))
	    (error "bytevector-fill!: " arg1 " is not a bytevector.")
	    (error "bytevector-fill!: " arg2 " is not a byte.")))

       ;; Bytevector-like

       ((= code $ex.mkbvl)
	(error "make-bytevector-like: "
	       arg1 " is not an exact nonnegative integer."))

       ((or (= code $ex.bvlref) (= code $ex.bvlset))
	(dstruct code $ex.bvlref "bytevector-like" bytevector-like? 
		 bytevector-like-length
		 arg1 arg2 byte? arg3))

       ((= code $ex.bvllen)
	(error "bytevector-like-length: " arg1 " is not a bytevector-like."))


       ;; typetags

       ((= code $ex.typetag)
	(error "typetag: " arg1 " does not have a typetag."))
       ((= code $ex.typetagset)
	;; FIXME: Tag range?
	(if (not (fixnum? arg2))
	    (error "typetag-set!: " arg2 " is an invalid typetag.")
	    (error "typetag-set!: " arg1 " is not typetag-settable.")))

       ;; Characters

       ((= code $ex.char2int)
	(error "char->integer: " arg1 " is not a character."))
       ((= code $ex.int2char)
	(error "integer->char: " arg1 " is not an exact nonnegative integer."))
       ((= code $ex.char<?)
	(charpred "char<?" arg1 arg2))
       ((= code $ex.char<=?)
	(charpred "char<=?" arg1 arg2))
       ((= code $ex.char=?)
	(charpred "char=?" arg1 arg2))
       ((= code $ex.char>?)
	(charpred "char>?" arg1 arg2))
       ((= code $ex.char>=?)
	(charpred "char>=?" arg1 arg2))

       ;; Others

       ;; for argument count exception, the supplied args are in RESULT,
       ;; the expected args (or the fixed args, for vargc) are in ARGREG2,
       ;; and the procedure being called is in ARGREG3.

       ((= code $ex.argc)
	(error "Wrong number of arguments to procedure " arg3))

       ((= code $ex.vargc)
	(error "Wrong number of arguments to procedure " arg3))

       ((= code $ex.apply)
	(cond ((not (procedure? arg1))
	       (error "Apply: non-procedure (ex): " arg1))
	      ((not (list? arg2))
	       (error "Apply: not a proper list: " arg2))
	      (else
	       (error "Apply: I'm sooo confused..."))))

       ((= code $ex.nonproc)
	(if print-object?
	    (error "Attempt to apply " arg1 ", which is not a procedure.")
	    (error "Attempt to apply non-procedure.")))

       ;; The pointer to the global Cell should now be in arg1 (RESULT).
       ;; Since a cell is a pair (currently!), and we know that the CDR
       ;; of the pair has some kind of ident for the global, we grab the
       ;; ident and print it.

       ((= code $ex.undef-global)
	(if (pair? arg1)
	    (error "Undefined global variable \"" (cdr arg1) "\".")
	    (error "Undefined global variable [doesn't look like a cell].")))

       ;; Created by the sys$dump primop. (Obsolete)

       ((= code $ex.dump)
	(cond ((not (procedure? arg2))
	       (error "sys$dump: " arg2 " is not a procedure."))
	      ((not (string? arg1))
	       (error "sys$dump: " arg1 " is not a string."))
	      (else
	       (error "sys$dump: filename too long: " arg1))))

       ((= code $ex.dumpfail)
	(error "sys$dump: dump failed."))

       ((= code $ex.unsupported)
	(error "Unsupported primitive " arg1))

       (else
	(error "exception-handler: Unhandled code: " code))))

    handler))

; eof
