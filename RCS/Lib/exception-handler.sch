; Larceny run-time library
; Your Basic Exception Handler in a Massive Case Statement (YBEHMCS).
;
; $Id$
;
; The procedure "exception-handler" takes the contents of RESULT, ARGREG2, and
; ARGREG3, and an exception code as arguments. It dispatches on the code and
; the contents of the three registers to provide a useful error message to
; the user; error is then called with the pertinent arguments.
;
; The procedure "print-object" takes a boolean and control whether the object
; in error is to be printed with the error message. This resolution may be
; a little coarse; it works for now.

(define exception-handler #f)
(define print-object #f)

(let ()

  ;; If ``print-object?'' is true then the faulting object is printed as part
  ;; of the error message, otherwise it is not printed.

  (define print-object? #t)

  (define (not-a-pair name obj)
    (if print-object?
	(error name ":" obj "is not a pair.")
	(error name ": not a pair.")))

  (define (not-a-num name obj)
    (if print-object?
	(error (string-append name ":") obj "is not a number.")
	(error (string-append name ": not a number."))))

  (define (not-a-real name obj)
    (if print-object?
	(error (string-append name ":") obj "is not a real number.")
	(error (string-append name ": not a real number."))))

  (define (not-an-int name obj)
    (if print-object?
	(error (string-append name ":") obj "is not an integer.")
	(error (string-append name ": not an integer."))))

  (define (not-a-fix name obj)
    (if print-object?
	(error (string-append name ":") obj "is not a fixnum.")
	(error (string-append name ": not a fixnum."))))

  (define (num-binop name arg1 arg2)
     (cond ((not (number? arg1)) (not-a-num name arg1))
	   ((not (number? arg2)) (not-a-num name arg2))
	   (else (error "exception-handler: confused about" name))))

  (define (real-binop name arg1 arg2)
     (cond ((not (real? arg1)) (not-a-real name arg1))
	   ((not (real? arg2)) (not-a-real name arg2))
	   (else (error "exception-handler: confused about" name))))

  (define (int-binop name arg1 arg2)
     (cond ((not (integer? arg1)) (not-an-int name arg1))
	   ((not (integer? arg2)) (not-an-int name arg2))
	   (else (error "exception-handler: confused about" name))))

  (define (fix-binop name arg1 arg2)
     (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
	   ((not (fixnum? arg2)) (not-a-fix name arg2))
	   (else (error "exception-handler: confused about" name))))

  (define (dstruct code reffer thing test? length)
    (let ((name (string-append thing (if (= code reffer) "-ref:" "-set!:"))))
      (cond ((not (test? arg1))
	     (if print-object?
		 (error name arg1 "is not a" thing)
		 (error name "not a" thing)))
	    ((or (not (fixnum? arg2))
		 (< arg2 0)
		 (>= arg2 (length arg1)))
	     (if print-object?
		 (error name arg2 "is not a valid index into" thing)
		 (error name "invalid index into" thing)))
	     
	    (else
	     (error "Exception-handler: confused about" name)))))


  (define (handler arg1 arg2 arg3 code)
    (cond 

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
       (num-binop "/" arg1 arg2))
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
       (int-binop "quotient" arg1 arg2))
      ((= code $ex.remainder)
       (int-binop "remainder" arg1 arg2))
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
      ((= code $ex.abs)
       (if (not (number? arg1))
	   (not-a-num "abs" arg1)
	   (not-a-real "abs" arg1)))
      ((= code $ex.neg)
       (not-a-num "-" arg1))
      ((= code $ex.realpart)
       (not-a-num "real-part" arg1))
      ((= code $ex.imagpart)
       (not-a-num "imag-part" arg1))
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

      ;; Vectors

      ((or (= code $ex.vref) (= code $ex.vset))
       (dstruct code $ex.vref "vector" vector? vector-length))

      ((= code $ex.vlen)
       (error "Vector-length:" arg1 "is not a vector"))

      ;; Procedures

      ((or (= code $ex.pref) (= code $ex.pset))
       (dstruct code $ex.pref "procedure" procedure? procedure-length))
      
      ((= code $ex.plen)
       (error "Procedure-length:" arg1 "is not a procedure."))

      ;; Vector-like

      ((or (= code $ex.vlref) (= code $ex.vlset))
       (dstruct code $ex.vlref "vector-like" vector-like? vector-like-length))
      
      ((= code $ex.vllen)
       (error "vector-like-length:" arg1 "is not a vecrtor-like."))

      ;; Strings

      ((or (= code $ex.sref) (= code $ex.sset))
       (dstruct code $ex.sref "string" string? string-length))

      ((= code $ex.slen)
       (error "string-length:" arg1 "is not a string"))

      ;; Bytevectors

      ((or (= code $ex.bvref) (= code $ex.bvset))
       (dstruct code $ex.bvref "bytevector" bytevector? bytevector-length))

      ((= code $ex.bvlen)
       (error "bytevector-length:" arg1 "is not a bytevector."))
      
      ;; Bytevector-like

      ((or (= code $ex.bvlref) (= code $ex.bvlset))
       (dstruct code $ex.bvlref "bytevector-like" bytevector-like? 
		                                  bytevector-like-length))

      ((= code $ex.bvllen)
       (error "bytevector-like-length:" arg1 "is not a bytevector-like."))

      ;; Others

      ;; for argument count exception, the supplied args are in RESULT,
      ;; the expected args (or the fixed args, for vargc) are in ARGREG2,
      ;; and the procedure being called is in ARGREG3.

      ((= code $ex.argc)
       (error "wrong number of arguments to procedure" arg3))

      ((= code $ex.vargc)
       (error "wrong number of arguments to procedure" arg3))

      ((= code $ex.apply)
       (error "Apply: kaboom."))

      ((= code $ex.typetag)
       (error "typetag:" arg1 "does not have a typetag."))

      ((= code $ex.typetagset)
       (if (not (fixnum? arg2))
	   (error "typetag-set!:" arg2 "is an invalid typetag.")
	   (error "typetag-set!:" arg1 "is not typetag-settable.")))

      ((= code $ex.nonproc)
       (if print-object?
	   (error arg1 "is not a procedure.")
	   (error "Attempt to apply non-procedure.")))

      ;; The pointer to the global Cell should now be in arg1 (RESULT).
      ;; Since a cell is a pair (currently!), and we know that the CDR
      ;; of the pair has some kind of ident for the global, we grab the
      ;; ident and print it.

      ((= code $ex.undef-global)
       (if (pair? arg1)
	   (error "Undefined global variable" (cdr arg1))
	   (error "Undefined global variable (doesn't look like a cell).")))

      (else
       (error "exception-handler: Unhandled code" code))))

  (set! exception-handler handler)
  (set! print-object (lambda a
		       (if (null? a)
			   print-object?
			   (set! print-object? (car a)))))

  #t)

; eof


    