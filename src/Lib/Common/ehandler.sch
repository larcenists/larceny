; Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; System-level handling of interrupts, breaks, signals, and primitive errors.

($$trace "ehandler")

; EXCEPTION-HANDLER takes the contents of RESULT, SECOND, and THIRD, and
; an exception code as arguments.  The code denotes either a primitive
; error or one of several kinds of interrupts.  EXCEPTION-HANDLER calls
; the handler that is appropriate for the code.
;
; EXCEPTION-HANDLER does not manipulate the state of the interrupt flag,
; leaving that to the handlers themselves.

; NOTE. Keyboard interrupt handling in this procedure will eventually be 
; handled by generalized signal handling.

(define (exception-handler arg1 arg2 arg3 code)
  (cond ((eqv? code $ex.timer)              ; Interrupt state is OFF
         ((timer-interrupt-handler)))
        ((eqv? code $ex.keyboard-interrupt) ; Interrupt state is unknown
         ((keyboard-interrupt-handler)))
        ((eqv? code $ex.breakpoint)         ; Interrupt state is unknown
         ((break-handler) arg1 arg2))
        ((eqv? code $ex.signal)             ; Interrupt state is unknown
         ((system-signal-handler) arg1))
        (else                            ; Interrupt state is unknown
         ((error-handler) code arg1 arg2 arg3))))

; The error handler takes a key as the first argument and then some
; additional arguments.  There are three cases, depending on the key:
;  - a number:  The error is a primitive error.  There will be three
;               additional values, the contents of RESULT, SECOND, and
;               THIRD.
;  - null:      The key is to be ignored, and the following arguments are
;               to be interpreted as a user-level error: objects to be
;               printed.
;  - otherwise: The arguments are to be interpreted as a user-level error:
;               objects to be printed.
; By design, the list of all arguments to the error handler may be
; passed as the first argument to DECODE-ERROR, in ERROR.SCH.
;
; Documented behavior: the default error handler prints all its
; arguments safely and then calls reset. The error handler may not 
; return to noncontinuable errors.  Currently all errors are 
; noncontinuable.

(define error-handler
  (make-parameter "error-handler" 
                  (lambda args 
                    (parameterize ((print-length 7)
                                   (print-level 7))
                      (decode-error args))
                    (reset))))

; The reset handler is called by the RESET procedure.  It takes no arguments.
;
; Documented behavior: the default reset handler exits to the operating
; system.  The reset handler may not return.

(define reset-handler
  (make-parameter "reset-handler" 
                  (lambda ignored
                    (exit))))

; The timer interrupt handler is called with timer interrupts OFF.
; It takes no arguments.

(define timer-interrupt-handler
  (make-parameter "timer-interrupt-handler"
                  (lambda ()
                    ($$debugmsg "Unhandled timer interrupt."))
                  procedure?))

; The keyboard interrupt handler is called when a keyboard interrupt has
; been seen.  It takes no arguments.

(define keyboard-interrupt-handler
  (make-parameter "keyboard-interrupt-handler"
                  (lambda ()
                    ($$debugmsg "Unhandled keyboard interrupt.")
                    (exit))
                  procedure?))

; The breakpoint handler is called when a BREAK primitive is encountered,
; and it may also be called by debugger-installed breakpoints that do not
; use the BREAK primitive.
;
; The breakpoint handler takes two arguments: the procedure in which the
; breakpoint occurred, and the code address at which it occurred.
; (Interpretation of the code address is architecture dependent.)

(define break-handler
  (make-parameter "break-handler"
                  (lambda (proc code-address)
                    (let ((out (current-error-port)))
                      (display "Breakpoint: " out)
                      (display proc out)
                      (display " @ " out)
                      (display code-address out)
                      (newline out)))))

; The system signal handler is called when an asynchronous signal is received
; for which a lowlevel handler has been installed.  It takes one argument:
; the signal.
;
; The meaning and representation of the signal are operating-system dependent.
; On Unix, it is a small nonnegative exact integer -- the signal number.

(define system-signal-handler
  (make-parameter "system-signal-handler"
                  (lambda (sig)
                    (let ((out (current-error-port)))
                      (display "Signal: " out)
                      (display sig out)
                      (newline out)))))

; DECODE-SYSTEM-ERROR takes an exception code and the exception argument
; values (RESULT, SECOND, THIRD), and a port onto which to print, and
; prints a human-readable error message on the port, explaining the error.  
; The output will be terminated by a newline.

(define (decode-system-error code arg1 arg2 arg3 port)

  (define (error . args)
    (newline port) ;; Ensure error message is easily visible.
    (display "Error: " port)
    (do ((args args (cdr args)))
        ((null? args) (newline port) (flush-output-port port))
      (display (car args) port)))

  (define (not-a-pair name obj)
    (error name ": " obj " is not a pair."))

  (define (not-a-num name obj)
    (error (string-append name ": ") obj " is not a number."))

  (define (not-a-real name obj)
    (error (string-append name ": ") obj " is not a real number."))

  (define (not-an-int name obj)
    (error (string-append name ": ") obj " is not an integer."))

  (define (div-by-zero name obj1 obj2)
    (error (string-append name ": division by zero: ") obj1 " " obj2))

  (define (not-a-fix name obj)
    (error (string-append name ": ") obj " is not a fixnum."))

  (define (not-a-flo name obj)
    (error (string-append name ": ") obj " is not a flonum."))

  (define (num-binop name arg1 arg2)
    (cond ((not (number? arg1)) (not-a-num name arg1))
          ((not (number? arg2)) (not-a-num name arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (num-div-binop name arg1 arg2)
    (cond ((not (number? arg1)) (not-a-num name arg1))
          ((not (number? arg2)) (not-a-num name arg2))
          ((zero? arg2) (div-by-zero name arg1 arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (real-binop name arg1 arg2)
    (cond ((not (real? arg1)) (not-a-real name arg1))
          ((not (real? arg2)) (not-a-real name arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (int-binop name arg1 arg2)
    (cond ((not (integer? arg1)) (not-an-int name arg1))
          ((not (integer? arg2)) (not-an-int name arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (int-div-binop name arg1 arg2)
    (cond ((not (integer? arg1)) (not-an-int name arg1))
          ((not (integer? arg2)) (not-an-int name arg2))
          ((zero? arg2) (div-by-zero name arg1 arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (fix-binop name arg1 arg2 . rest)
    (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
          ((not (fixnum? arg2)) (not-a-fix name arg2))
          ((and (not (null? rest))
                (not (fixnum? ((car rest) arg1 arg2))))
           (raise-r6rs-exception (make-implementation-restriction-violation)
                                 name
                                 (errmsg 'msg:fixnumrange)
                                 (list arg1 arg2)))
          (else (error "decode-system-error: confused about " name))))

  (define (fix-unop name arg1 . rest)
    (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
          ((and (not (null? rest))
                (not (fixnum? ((car rest) arg1))))
           (raise-r6rs-exception (make-implementation-restriction-violation)
                                 name
                                 (errmsg 'msg:fixnumrange)
                                 (list arg1)))
          (else (error "decode-system-error: confused about " name))))

  (define (flo-binop name arg1 arg2 . rest)
    (cond ((not (flonum? arg1)) (not-a-flo name arg1))
          ((not (flonum? arg2)) (not-a-flo name arg2))
          (else (error "decode-system-error: confused about " name))))

  (define (flo-unop name arg1 . rest)
    (cond ((not (flonum? arg1)) (not-a-flo name arg1))
          (else (error "decode-system-error: confused about " name))))

  (define (dstruct code reffer thing test? length arg1 arg2 . rest)
    (let ((name (string-append thing (if (= code reffer) "-ref:" "-set!:"))))
      (cond ((not (test? arg1))
             (error name " " arg1 " is not a " thing))
            ((or (not (fixnum? arg2))
                 (< arg2 0)
                 (>= arg2 (length arg1)))
             (error name " " arg2 " is not a valid index into " thing))
            ((and (not (null? rest))
                  (not (= code reffer))
                  (not ((car rest) (cadr rest))))
             (error name " " (cadr rest) " cannot be stored in a " thing))
            (else
             (if (bignum? arg1)
                 (begin (display "BIG: " port)
                        (bigdump* arg1 port)
                        (newline port)))
             (error "decode-system-error: confused about " 
                    name " " arg1 " " arg2)))))

  (define (charpred name arg1 arg2)
    (error name ": " (if (char? arg1) arg2 arg1) " is not a character."))

  (define (not-a-port name obj)
    (error name ": " obj " is not a port."))

  (define (byte? x)
    (and (integer? x) (exact? x) (<= 0 x 255)))

  (cond ;; Interrupt flag manipulation

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
       ((= code $ex.fl+)
        (flo-binop "fl+" arg1 arg2))
       ((= code $ex.fl-)
        (flo-binop "fl-" arg1 arg2))
       ((= code $ex.fl--)
        (flo-unop  "fl-" arg1 fl-))
       ((= code $ex.fl=)
        (flo-binop "fl=" arg1 arg2))
       ((= code $ex.fl<)
        (flo-binop "fl<" arg1 arg2))
       ((= code $ex.fl<=)
        (flo-binop "fl<=" arg1 arg2))
       ((= code $ex.fl>)
        (flo-binop "fl>" arg1 arg2))
       ((= code $ex.fl>=)
        (flo-binop "fl>=" arg1 arg2))
       ((= code $ex.fl*)
        (flo-binop "fl*" arg1 arg2))
       ((= code $ex.fl/)
        (flo-binop "fl/" arg1 arg2))
       ((= code $ex.logior)
        (fix-binop "fxlogior" arg1 arg2))
       ((= code $ex.logand)
        (fix-binop "fxlogand" arg1 arg2))
       ((= code $ex.logxor)
        (fix-binop "fxlogxor" arg1 arg2))
       ((= code $ex.lognot)
        (not-a-fix "fxlognot" arg1))
       ((= code $ex.lsh)
        (fix-binop "fxlsh" arg1 arg2))
       ((= code $ex.rsha)
        (fix-binop "fxrsha" arg1 arg2))
       ((= code $ex.rshl)
        (fix-binop "fxrshl" arg1 arg2))
       ((= code $ex.fx+)
        (fix-binop "fx+" arg1 arg2 +))
       ((= code $ex.fx-)
        (fix-binop "fx-" arg1 arg2 -))
       ((= code $ex.fx*)
        (fix-binop "fx*" arg1 arg2 *))
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
        (fix-unop "fx--" arg1 --))
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

       ;; Records

       ((= code $ex.record)
        (error "Record access: " arg1 " is not a " (rtd-name arg2)))

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
       ((= code $ex.mkstr)
        (if (char? arg2)
            (error "make-string: "
                   arg1 " is not an exact nonnegative integer.")
            (error "make-string: "
                   arg2 " is not a character.")))

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
        (error "integer->char: " arg1 " is not a Unicode scalar value."))
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

       ;; I/O

       ((= code $ex.get-u8)
        (not-a-port "get-u8" arg1))
       ((= code $ex.put-u8)
        (not-a-port "put-u8" arg1))
       ((= code $ex.get-char)
        (not-a-port "get-char" arg1))
       ((= code $ex.put-char)
        (not-a-port "put-char" arg1))

       ;; Others

       ;; For assertions, the first argument is the expression that
       ;; evaluated to false, and the second argument is #f or some
       ;; description of the source code location.

       ((= code $ex.assert)
        (if arg2
            (error "Assertion failed: " arg1 #\space arg2)
            (error "Assertion failed: " arg1)))

       ;; for argument count exception, the supplied args are in RESULT,
       ;; the expected args (or the fixed args, for vargc) are in ARGREG2,
       ;; and the procedure being called is in ARGREG3.

       ((= code $ex.argc)
        (error "Wrong number of arguments to procedure " arg3 #\, #\newline
               "    which expected " arg2 " but got " arg1))
       
       ((= code $ex.vargc)
        (error "Wrong number of arguments to procedure " arg3 #\, #\newline
               "    which expected at least " arg2 " but got " arg1))

       ((= code $ex.apply)
        (cond ((not (procedure? arg1))
               (error "Apply: non-procedure (ex): " arg1))
              ((not (list? arg2))
               (error "Apply: not a proper list: " arg2))
              (else
               (error "Apply: I'm sooo confused..."))))

       ((= code $ex.nonproc)
        (error "Attempt to apply " arg1 ", which is not a procedure."))

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
       ;;
       ;; FIXME: it would be possible for the nonproc case to print the
       ;; name of the global variable that does not contain a procedure.

       ((= code $ex.global-invoke)
        (if (eq? arg1 (undefined))
            (decode-system-error $ex.undef-global arg2 #f #f port)
            (decode-system-error $ex.nonproc arg1 arg2 arg3 port)))

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
        (error "decode-system-error: Unhandled code: " code))))


; FIXME: OS-dependent, belongs in its own file.

(define (interpret-arithmetic-exception-code code)
  (let ((os-name  (cdr (assq 'os-name (system-features))))
        (os-major (cdr (assq 'os-major-version (system-features)))))
    (if (string=? os-name "SunOS")
        (case os-major
          ((4) (case code
                 ((#x14) 'intdiv)
                 ((#xC4) 'fltres)
                 ((#xC8) 'fltdiv)
                 ((#xCC) 'fltund)
                 ((#xD0) 'fltopr)
                 ((#xD4) 'fltovf)
                 (else #f)))
          ((5) (case code
                 ((1) 'intdiv)
                 ((2) 'intovf)
                 ((3) 'fltdiv)
                 ((4) 'fltovf)
                 ((5) 'fltund)
                 ((6) 'fltres)
                 ((7) 'fltinv)
                 ((8) 'fltsub)
                 (else #f)))
          (else #f))
        #f)))

; eof
