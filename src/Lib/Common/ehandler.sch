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
                    (reset))
                  procedure?))

; The reset handler is called by the RESET procedure.  It takes no arguments.
;
; Documented behavior: the default reset handler exits to the operating
; system.  The reset handler may not return.

(define reset-handler
  (make-parameter "reset-handler" 
                  (lambda ignored
                    (exit))
                  procedure?))

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
                      (newline out)))
                  procedure?))

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
                      (newline out)))
                  procedure?))

; DECODE-SYSTEM-ERROR takes an exception code and the exception argument
; values (RESULT, SECOND, THIRD), and a port onto which to print, and
; prints a human-readable error message on the port, explaining the error.  
; The output will be terminated by a newline.

(define (decode-system-error code arg1 arg2 arg3 port)

  ;; This local version of the error procedure does not raise an exception.
  ;; It just prints an error message to the port.
  ;; Its arguments are similar to those passed to the R6RS error procedure,
  ;; but this version gives special treatment to the empty message ""
  ;; and treats string and list irritants specially:
  ;; strings are treated as infix messages, lists as lists of irritants.
  ;;
  ;; The printed error message will take one of the forms
  ;;
  ;;     Error: <who>: <irritant-or-text> ...
  ;;     Error: <who>: <msg>: <irritant-or-text> ...
  ;;
  ;; The decode-error hack in error.sch interprets that output,
  ;; so don't change it here without making a corresponding change
  ;; to decode-error.

  (define (error who msg . args)
    (newline port) ;; Ensure error message is easily visible.
    (display "Error: " port)
    (if who
        (display who port)
        (display "?" port))
    (display ": " port)
    (if (not (string=? msg ""))
        (begin (display msg port)
               (display ": " port)))
    (for-each (lambda (arg)
                (cond ((string? arg)
                       (display arg port))
                      ((list? arg)
                       (for-each (lambda (x)
                                   (write x port)
                                   (display " " port))
                                 arg))
                      (else
                       (write arg port)
                       (display " " port))))
              args)
    (newline port)
    (flush-output-port port))

  ;; This has to be kept in sync with lib/R6RS/r6rs-compat-larceny.sch

  (define (unmangle-global-variable var)
    (or (and (symbol? var)
             (let ((s (symbol->string var)))
               (and (not (string=? s ""))
                    (char=? #\x1 (string-ref s 0))
                    (string->symbol (substring s 1 (string-length s))))))
        var))

  (define (old-error . args)
    (newline port) ;; Ensure error message is easily visible.
    (display "Error: " port)
    (do ((args args (cdr args)))
        ((null? args) (newline port) (flush-output-port port))
      (display (car args) port)))

  (define (not-a-pair name obj)
    (error name (errmsg 'msg:notpair) (list obj)))

  (define (not-a-num name obj)
    (error name (errmsg 'msg:notnumber) (list obj)))

  (define (not-a-real name obj)
    (error name (errmsg 'msg:notreal) (list obj)))

  (define (not-an-int name obj)
    (error name (errmsg 'msg:notinteger) (list obj)))

  (define (div-by-zero name obj1 obj2)
    (error name (errmsg 'msg:zerodivide) (list obj1 obj2)))

  (define (not-a-fix name obj)
    (error name (errmsg 'msg:notfixnum) (list obj)))

  (define (not-a-flo name obj)
    (error name (errmsg 'msg:notflonum) (list obj)))

  (define (num-binop name arg1 arg2)
    (cond ((not (number? arg1)) (not-a-num name arg1))
          ((not (number? arg2)) (not-a-num name arg2))
          (else (error name (errmsg 'msg:confused) (list arg1 arg2)))))

  (define (num-div-binop name arg1 arg2)
    (cond ((not (number? arg1)) (not-a-num name arg1))
          ((not (number? arg2)) (not-a-num name arg2))
          ((zero? arg2) (div-by-zero name arg1 arg2))
          (else (error name (errmsg 'msg:confused) (list arg1 arg2)))))

  (define (real-binop name arg1 arg2)
    (cond ((not (real? arg1)) (not-a-real name arg1))
          ((not (real? arg2)) (not-a-real name arg2))
          (else (error name (errmsg 'msg:confused) (list arg1 arg2)))))

  (define (int-binop name arg1 arg2)
    (cond ((not (integer? arg1)) (not-an-int name arg1))
          ((not (integer? arg2)) (not-an-int name arg2))
          (else (error name (errmsg 'msg:confused) (list arg1 arg2)))))

  (define (int-div-binop name arg1 arg2)
    (cond ((not (integer? arg1)) (not-an-int name arg1))
          ((not (integer? arg2)) (not-an-int name arg2))
          ((zero? arg2) (div-by-zero name arg1 arg2))
          (else (error name (errmsg 'msg:confused) (list arg1 arg2)))))

  (define (fix-binop name arg1 arg2 . rest)
    (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
          ((not (fixnum? arg2)) (not-a-fix name arg2))
          ((and (not (null? rest))
                (not (fixnum? ((car rest) arg1 arg2))))
           (raise-r6rs-exception (make-implementation-restriction-violation)
                                 name
                                 (errmsg 'msg:fixnumrange)
                                 (list arg1 arg2)))
          (else (error name (errmsg 'msg:confused)
                            (cons arg1 (cons arg2 rest))))))

  (define (fix-unop name arg1 . rest)
    (cond ((not (fixnum? arg1)) (not-a-fix name arg1))
          ((and (not (null? rest))
                (not (fixnum? ((car rest) arg1))))
           (raise-r6rs-exception (make-implementation-restriction-violation)
                                 name
                                 (errmsg 'msg:fixnumrange)
                                 (list arg1)))
          (else (error name (errmsg 'msg:confused)
                            (cons arg1 rest)))))

  (define (flo-binop name arg1 arg2 . rest)
    (cond ((not (flonum? arg1)) (not-a-flo name arg1))
          ((not (flonum? arg2)) (not-a-flo name arg2))
          (else (error name (errmsg 'msg:confused)
                            (cons arg1 (cons arg2 rest))))))

  (define (flo-unop name arg1 . rest)
    (cond ((not (flonum? arg1)) (not-a-flo name arg1))
          (else (error name (errmsg 'msg:confused)
                            (cons arg1 rest)))))

  (define (dstruct code reffer thing test? length arg1 arg2 . rest)
    (let ((name (string-append thing (if (= code reffer) "-ref" "-set!"))))
      (cond ((not (test? arg1))
             (error name (errmsg 'msg:illegalarg1)
                         (list arg1) " is not a " thing))
            ((or (not (fixnum? arg2))
                 (< arg2 0)
                 (>= arg2 (length arg1)))
             (error name (errmsg 'msg:illegalarg2)
                         (list arg2) " is not a valid index into " thing))
            ((and (not (null? rest))
                  (not (= code reffer))
                  (not ((car rest) (cadr rest))))
             (error name (errmsg 'msg:illegalarg3)
                         (list (cadr rest)) " cannot be stored in a " thing))
            (else
             ;; FIXME: what's this about?
             (if (bignum? arg1)
                 (begin (display "BIG: " port)
                        (bigdump* arg1 port)
                        (newline port)))
             (error name (errmsg 'msg:confused) (list arg1 arg2 rest))))))

  (define (charpred name arg1 arg2)
    (error name (errmsg 'msg:notchar) (list (if (char? arg1) arg2 arg1))))

  (define (not-a-port name obj)
    (error name (errmsg 'msg:notport) (list obj)))

  (define (byte? x)
    (and (integer? x) (exact? x) (<= -128 x 255)))

  (cond ;; Interrupt flag manipulation

       ((= code $ex.enable-interrupts)
        (cond ((not (fixnum? arg1))
               (error 'enable-interrupts (errmsg 'msg:notfixnum) (list arg1)))
              ((<= arg1 0)
               (error 'enable-interrupts (errmsg 'msg:notnaturalnumber)
                                         (list arg1)))
              (else
               (error 'enable-interrupts (errmsg 'msg:confused)
                      (list arg1 arg2 arg3)))))

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
        (flo-unop  "fl-" arg1))
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
       ((= code $ex.flfloor)
        (flo-unop "flfloor" arg1))
       ((= code $ex.flceiling)
        (flo-unop "flceiling" arg1))
       ((= code $ex.fltruncate)
        (flo-unop "fltruncate" arg1))
       ((= code $ex.flround)
        (flo-unop "flround" arg1))
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
          ((intdiv) (error #f (errmsg 'msg:zerodivide) (list arg1 arg2)))
          ((intovf) (error #f "integer overflow"))
          ((fltdiv) (error #f "floating point division by zero"
                              (list arg1 arg2)))
          ((fltovf) (error #f "floating point overflow"))
          ((fltund) (error #f "floating point underflow"))
          ((fltres) (error #f "floating point inexact result"))
          ((fltinv) (error #f "invalid floating point operation"))
          ((fltsub) (error #f "floating point subscript out of range"))
          ((fltopr) (error #f "floating point operand error"))
          (else (error #f "arithmetic exception" "(code " arg3 ")"))))
              
       ;; Vectors

       ((or (= code $ex.vref) (= code $ex.vset))
        (dstruct code $ex.vref "vector" vector? vector-length arg1 arg2))
       ((= code $ex.vlen)
        (error 'vector-length (errmsg 'msg:notvector) (list arg1)))

       ;; Procedures

       ((or (= code $ex.pref) (= code $ex.pset))
        (dstruct code $ex.pref "procedure" procedure? procedure-length
                 arg1 arg2))
       ((= code $ex.plen)
        (error 'procedure-length (errmsg 'msg:notproc) (list arg1)))

       ;; Records

       ((= code $ex.record)
        (error "record access" (errmsg 'msg:illegalarg1)
                               (list arg1) " is not a " (rtd-name arg2)))

       ;; Vector-like

       ((= code $ex.mkvl)
        (error 'make-vector-like (errmsg 'msg:nonnaturalnumber) (list arg1)))

       ((or (= code $ex.vlref) (= code $ex.vlset))
        (dstruct code $ex.vlref "vector-like" vector-like? vector-like-length
                 arg1 arg2))

       ((= code $ex.vllen)
        (error 'vector-like-length (errmsg 'msg:illegalarg1)
                                   (list arg1) " is not vector-like"))

       ;; Strings

       ((or (= code $ex.sref) (= code $ex.sset))
        (dstruct code $ex.sref "string" string? string-length arg1 arg2
                 char? arg3))
       ((= code $ex.slen)
        (error 'string-length (errmsg 'msg:notstring) (list arg1)))
       ((= code $ex.mkstr)
        (if (char? arg2)
            (error 'make-string (errmsg 'msg:nonnaturalnumber) (list arg1))
            (error 'make-string (errmsg 'msg:notchar) (list arg2))))

       ;; Bytevectors

       ((or (= code $ex.bvref) (= code $ex.bvset))
        (dstruct code $ex.bvref "bytevector" bytevector? bytevector-length
                 arg1 arg2 byte? arg3))
       ((= code $ex.bvlen)
        (error 'bytevector-length (errmsg 'msg:notbytevector) (list arg1)))
      
       ((= code $ex.bvfill)
        (if (not (bytevector? arg1))
            (error 'bytevector-fill! (errmsg 'msg:bytevector) (list arg1))
            (error 'bytevector-fill! (errmsg 'msg:byte) (list arg2))))

       ;; Bytevector-like

       ((= code $ex.mkbvl)
        (error 'make-bytevector-like (errmsg 'msg:notnaturalnumber)
                                     (list arg1)))

       ((or (= code $ex.bvlref) (= code $ex.bvlset))
        (dstruct code $ex.bvlref "bytevector-like" bytevector-like? 
                 bytevector-like-length
                 arg1 arg2 byte? arg3))

       ((= code $ex.bvllen)
        (error 'bytevector-like-length "not bytevector-like" (list arg1)))

       ;; typetags

       ((= code $ex.typetag)
        (error 'typetag (errmsg 'msg:illegalarg1)
                        (list arg1) " does not have a typetag."))
       ((= code $ex.typetagset)
        (if (not (and (fixnum? arg2)
                      (<= 0 arg2 7)))
            (error 'typetag-set! (errmsg 'msg:illegalarg2)
                                 (list arg2) " is an invalid typetag")
            (error 'typetag-set! (errmsg 'msg:illegalarg1)
                                 (list arg1) " is not typetag-settable")))

       ;; Characters

       ((= code $ex.char2int)
        (error 'char->integer (errmsg 'msg:notchar) (list arg1)))
       ((= code $ex.int2char)
        (error 'integer->char (errmsg 'msg:illegalarg1)
                              (list arg1) " is not a Unicode scalar value"))
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

       ;; Allocation

       ;; The allocation size is given in words, and it may be negative
       ;; due to overflow when adding a vector header and/or aligning.
       ;; We report the unsigned value in bytes.

       ((= code $ex.alloc)
        (let ((uwords (if (< arg1 0)
                        (+ arg1 (expt 2 30))
                        arg1)))
          (error #f (errmsg 'msg:alloctoobig) (list (* uwords 4)))))

       ;; Others

       ;; For assertions, the first argument is the expression that
       ;; evaluated to false, and the second argument is #f or some
       ;; description of the source code location.

       ((= code $ex.assert)
        (error #f (errmsg 'msg:assert)
                  (if arg2 (list arg1 arg2) (list arg1))))

       ;; for argument count exception, the supplied args are in RESULT,
       ;; the expected args (or the fixed args, for vargc) are in ARGREG2,
       ;; and the procedure being called is in ARGREG3.

       ((= code $ex.argc)
        (let ((name (and (procedure? arg3) (procedure-name arg3))))
          (error name (errmsg 'msg:wna)
                      "\n"
                      "    expected " (list arg2) " but got " (list arg1))))
       
       ((= code $ex.vargc)
        (let ((name (and (procedure? arg3) (procedure-name arg3))))
          (error name (errmsg 'msg:wna)
                      "\n"
                      "    expected at least " (list arg2)
                      " but got " (list arg1))))

       ((= code $ex.apply)
        (cond ((not (procedure? arg1))
               (error 'apply (errmsg 'msg:notproc) (list arg1)))
              ((not (list? arg2))
               (error 'apply (errmsg 'msg:notlist) (list arg2)))
              (else
               (error 'apply (errmsg 'msg:confused)))))

       ((= code $ex.nonproc)
        (error #f (errmsg 'msg:notproc) "tried to call " (list arg1)))

       ;; The pointer to the global Cell should now be in arg1 (RESULT).
       ;; Since a cell is a pair (currently!), and we know that the CDR
       ;; of the pair has some kind of ident for the global, we grab the
       ;; ident and print it.

       ((= code $ex.undef-global)
        (if (pair? arg1)
            (error #f "undefined global variable"
                      (list (unmangle-global-variable (cdr arg1))))
            (error #f "undefined global variable [doesn't look like a cell]")))

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
               (error "sys$dump" (errmsg 'msg:notproc) (list arg2)))
              ((not (string? arg1))
               (error "sys$dump" (errmsg 'msg:notstring) (list arg1)))
              (else
               (error "sys$dump" "filename too long" (list arg1)))))

       ((= code $ex.dumpfail)
        (error "sys$dump" "dump failed"))

       ((= code $ex.unsupported)
        (error #f "unsupported primitive" (list arg1)))

       (else
        (error "decode-system-error" "unhandled code" (list code)))))


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
