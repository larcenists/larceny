; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Simple exception system -- prototype.
;
; There are three kinds of exceptions:
;  - System exceptions are signalled by system primitives and are
;    noncontinuable.
;  - Errors are signalled by the ERROR procedure and are noncontinuable.
;  - User exceptions are created with the MAKE-EXCEPTION procedure and
;    signalled by the SIGNAL-EXCEPTION procedure.  They are continuable
;    or not.
;
; (make-exception type values context continuable?) => exn
;   TYPE is a symbol: "system", "error", or "user".  If "user", then
;   values can be any list of values, and context should for the time
;   being be #f.
;
; (exception? object) => boolean
;   Tests whether the object is an exception object.
;
; (exception-type exn) => symbol
;   Returns the type of the exception object EXN.
;
; (exception-values exn) => list
;   Returns the list of exception value in the exception object EXN.
;
; (exception-context exn) => object
;   Returns the context object of the exception object EXN.
;
; (exception-continuable? exn) => boolean
;   EXN must be an exception object.  Returns #t if one can return from the
;   exception handler and expect something reasonable to happen.
;
; (exception-message exn) => string
;   EXN must be an exception object.  A human-readable string explaining the
;   exception is returned.
;
; (call-with-handler handler thunk) => values
;   Calls THUNK with no arguments.  If THUNK signals an exception exn
;   then HANDLER is invoked on exn in the dynamic context of the exception.
;
; (call-with-exception-handler handler thunk) => values
;   Calls THUNK with no arguments.  If THUNK signals an exception exn
;   then HANDLER is invoked on exn in the dynamic context of the caller
;   of CALL-WITH-EXCEPTION-HANDLER.  (Note: a poorly chosen name, not
;   yet corrected because Doug uses this package.)
;
; (error object ...)
;   Signal a noncontinuable error exception where "object ..." are the
;   exception values.
;
; (signal-exception exn) => values
;   EXN must be an exception object.  The exception is signalled, and any
;   the closest handler in the dynamic scope is invoked.  If the exception
;   is continuable, and the handler returns, then the values returned by
;   the handler are those returned by signal-exception.  If the exception
;   is noncontinuable, then signal-exception does not return.
;
;
; For example, here's a nested handler and re-raise:
;
;  (call-with-exception-handler
;    (lambda (exn)
;      (format #t "Error: ~a~%" (exception-message exn))
;      (reset))
;    (lambda ()
;      (call-with-exception-handler
;        (lambda (exn)
;  	   (signal-exception exn))
;        (lambda () 
;          (error "This is a test.")))))

'(require 'macros)                      ; PARAMETERIZE and other syntax
'(require 'record)                      ; Record system (used by DEFINE-RECORD)
'(require 'define-record)               ; DEFINE-RECORD syntax

(define exception/token (vector 'exception))

(define-record exception (type values context continuable?))

(define (exception-message exn)
  (case (exception-type exn)
    ((system)
     (apply exception/decode-system-exception (exception-values exn)))
    ((error)
     (exception/decode-error-exception (exception-values exn)))
    ((user)
     (exception/decode-error-exception 
      (list "User exception: " (exception-values exn))))
    (else ???)))

(define (call-with-handler handler thunk)
  (parameterize 
      ((error-handler
        (lambda (who . args)
          (call-with-current-continuation
           (lambda (context)
             (let ((exn
                    (cond ((eq? who exception/token)
                           (car args))
                          ((number? who)
                           (make-exception 'system (cons who args) context #f))
                          ((null? who)
                           (make-exception 'error args context #f))
                          (else
                           (make-exception 'error (cons who (cons ": " args)) 
                                           context
                                           #f)))))
               (if (not (exception-continuable? exn))
                   (begin
                     (handler exn)
                     (error "Handler for non-continuable exception returned."))
                   (handler exn))))))))
    (thunk)))

(define (call-with-exception-handler handler thunk)
  (let* ((exn #f)
         (r (call-with-current-continuation
             (lambda (escape)
               (call-with-handler
                (lambda (e)
                  (set! exn e)
                  (escape #f))
                thunk)))))
    (if exn
        (handler exn)
        r)))

(define (signal-exception exn)
  ((error-handler) exception/token exn))


; Internal

(define (exception/decode-error-exception values)
  (let ((s (open-output-string)))
    (for-each (lambda (x) (display x s)) values)
    (get-output-string s)))

; Stolen from Lib/Common/ehandler.sch and Lib/Common/ecodes.sch and modified.

(define exception/decode-system-exception
  (let ()

    (define $ex.car 0)
    (define $ex.cdr 1)
    (define $ex.setcar 2)
    (define $ex.setcdr 3)
    (define $ex.add 10)
    (define $ex.sub 11)
    (define $ex.mul 12)
    (define $ex.div 13)
    (define $ex.lessp 14)
    (define $ex.lesseqp 15)
    (define $ex.equalp 16)
    (define $ex.greatereqp 17)
    (define $ex.greaterp 18)
    (define $ex.quotient 19)
    (define $ex.remainder 20)
    (define $ex.modulo 21)
    (define $ex.logior 22)
    (define $ex.logand 23)
    (define $ex.logxor 24)
    (define $ex.lognot 25)
    (define $ex.lsh 26)
    (define $ex.rsha 27)
    (define $ex.rshl 28)
    (define $ex.e2i 29)
    (define $ex.i2e 30)
    (define $ex.exactp 31)
    (define $ex.inexactp 32)
    (define $ex.round 33)
    (define $ex.trunc 34)
    (define $ex.zerop 35)
    (define $ex.neg 36)
    (define $ex.abs 37)
    (define $ex.realpart 38)
    (define $ex.imagpart 39)
    (define $ex.vref 40)
    (define $ex.vset 41)
    (define $ex.vlen 42)
    (define $ex.pref 50)
    (define $ex.pset 51)
    (define $ex.plen 52)
    (define $ex.sref 60)
    (define $ex.sset 61)
    (define $ex.slen 62)
    (define $ex.bvref 70)
    (define $ex.bvset 71)
    (define $ex.bvlen 72)
    (define $ex.bvlref 80)
    (define $ex.bvlset 81)
    (define $ex.bvllen 82)
    (define $ex.vlref 90)
    (define $ex.vlset 91)
    (define $ex.vllen 92)
    (define $ex.typetag 100)
    (define $ex.typetagset 101)
    (define $ex.apply 102)
    (define $ex.argc 103)
    (define $ex.vargc 104)
    (define $ex.nonproc 105)
    (define $ex.undef-global 106)
    (define $ex.dump 107)
    (define $ex.dumpfail 108)
    (define $ex.timer 109)
    (define $ex.unsupported 110)
    (define $ex.int2char 111)
    (define $ex.char2int 112)
    (define $ex.mkbvl 113)
    (define $ex.mkvl 114)
    (define $ex.char<? 115)
    (define $ex.char<=? 116)
    (define $ex.char=? 117)
    (define $ex.char>? 118)
    (define $ex.char>=? 119)
    (define $ex.bvfill 120)
    (define $ex.enable-interrupts 121)
    (define $ex.keyboard-interrupt 122)
    (define $ex.arithmetic-exception 123)
    (define $ex.global-invoke 124)
    (define $ex.fx+ 140)
    (define $ex.fx- 141)
    (define $ex.fx-- 142)
    (define $ex.fx= 143)
    (define $ex.fx< 144)
    (define $ex.fx<= 145)
    (define $ex.fx> 146)
    (define $ex.fx>= 147)
    (define $ex.fxpositive? 148)
    (define $ex.fxnegative? 149)
    (define $ex.fxzero? 150)

    (define print-object? #t)

    (define (error . args)
      (exception/decode-error-exception args))

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
      (error (string-append name ": division by zero: ") obj1 " " obj2))

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

    (define (fix-unop name arg1)
      (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
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
	       (error name " " (cadr rest) " cannot be stored in a " thing))
	      (else
	       (if (bignum? arg1)
		   (begin (display "BIG: ") (bigdump* arg1) (newline)))
	       (error "Exception-handler: confused about " 
		      name " " arg1 " " arg2)))))

    (define (charpred name arg1 arg2)
      (error name ": " (if (char? arg1) arg2 arg1) " is not a character."))

    (define (byte? x)
      (and (integer? x) (exact? x) (<= 0 x 255)))

    (define (system-decoder code arg1 arg2 arg3)
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
	(not-a-pair "set-car!" arg1))
       ((= code $ex.setcdr)
	(not-a-pair "set-cdr!" arg1))

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
       ((= code $ex.fx+)
	(fix-binop "fx+" arg1 arg2))
       ((= code $ex.fx-)
	(fix-binop "fx-" arg1 arg2))
       ((= code $ex.fx=)
	(fix-binop "fx=" arg1 arg2))
       ((= code $ex.fx<)
	(fix-binop "fx<" arg1 arg2))
       ((= code $ex.fx<=)
	(fix-binop "fx<=" arg1 arg2))
       ((= code $ex.fx>)
	(fix-binop "fx>" arg1 arg2))
       ((= code $ex.fx>=)
	(fix-binop "fx>=" arg1 arg2))
       ((= code $ex.fx--)
	(fix-unop "fx--" arg1))
       ((= code $ex.fxnegative?)
	(fix-unop "fxnegative?" arg1))
       ((= code $ex.fxpositive?)
	(fix-unop "fxpositive?" arg1))
       ((= code $ex.fxzero?)
	(fix-unop "fxzero?" arg1))
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
	(not-a-num "--" arg1))
       ((= code $ex.abs)
	(if (not (number? arg1))
	    (not-a-num "abs" arg1)
	    (not-a-real "abs" arg1)))
       ((= code $ex.realpart)
	(not-a-num "real-part" arg1))
       ((= code $ex.imagpart)
	(not-a-num "imag-part" arg1))
       ((= code $ex.arithmetic-exception)
	(case (interpret-arithmetic-exception-code arg3)
	  ((intdiv) (error "Integer division by zero: " arg1 " " arg2))
	  ((intovf) (error "Integer overflow."))
	  ((fltdiv) (error "Floating point division by zero: " arg1 " " arg2))
	  ((fltovf) (error "Floating point overflow."))
	  ((fltund) (error "Floating point underflow."))
	  ((fltres) (error "Floating point inexact result."))
	  ((fltinv) (error "Invalid floating point operation."))
	  ((fltsub) (error "Floating point subscript out of range."))
	  ((fltopr) (error "Floating point operand error."))
	  (else (error "Arithmetic exception (code " arg3 ")."))))
	      
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
	(if (not (and (fixnum? arg2)
		      (<= 0 arg2 7)))
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

       ;; Here the cell is in arg2 (SECOND); if arg1 (RESULT) is undefined,
       ;; then it's an undefined-global error, otherwise it's a non-procedure
       ;; error.

       ((= code $ex.global-invoke)
	(if (eq? arg1 (undefined))
	    (system-decoder $ex.undef-global arg2 #f #f)
	    (system-decoder $ex.nonproc arg1 arg2 arg3)))

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
	(error "system-decoder: Unhandled code: " code))))

    system-decoder))

; eof
