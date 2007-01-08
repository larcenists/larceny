; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Trace and breakpoint facilities for Larceny.
;
; (trace <proc>)                Trace <proc> at entry and exit
; (trace-entry <proc>)          Trace <proc> at entry
; (trace-exit <proc>)           Trace <proc> at exit
; (untrace <proc> ...)          Disable tracing for <procs>
; (untrace)                     Disable tracing for all traced <procs>
; trace-entry-handler           Parameter (String Sexp [Listof Symbol] -> Unspecified)
; trace-exit-handler            Parameter (String Sexp [Listof Symbol] [Listof Any] -> Unspecified)
;
; (larceny-break)               Break at this point (a primitive)
; (break-entry <proc>)          Break on entry to <proc>
; (unbreak <proc> ...)          Disable breaking for <procs>
; (unbreak)                     Disable breaking for all broken <procs>
;
; IDEA: It may be useful to allow TRACE, TRACE-ENTRY, TRACE-EXIT, 
;       BREAK-ENTRY to accept an optional name by which to identify 
;       the traced procedure, since anonymous procedures are otherwise 
;       hard to tell apart (other than by their code).
;
; IDEA: It is possible for BREAK-ENTRY to take an additional
;       argument which is a procedure that takes an environment
;       (the environment in which formals are bound to the values)
;       and is called when the breakpoint is encountered.  The procedure
;       returns #t if the breakpoint should trigger, #f if not.
;       The environment can be something as simple as an assoc list.
;       If formal names aren't available, one can use positional
;       parameters as the keys, or identifiers like _1, and _2.
;       In fact, with identifiers like that it is also possible to
;       pass an _expression_ that will be evaluated in that environment.
;
;       Something similar can currently be emulated with a user-installed
;       break-handler.
;
; FIXME: Not reentrant/multi-user-aware.  The traced/broken lists could be
;        parameters (thus thread-relative for suitable definition of 
;        parameter) and the handlers could lookup the procedures on the 
;        traced/broken lists before reporting a trace/breakpoint.

(define trace-level 
  (make-parameter "trace-level" 0))

(define trace-entry-handler
  (make-parameter "trace-entry-handler" (lambda (name expr args)
					  (debug/default-trace-entry-handler name expr args))))

(define trace-exit-handler
  (make-parameter "trace-exit-handler" (lambda (name expr args results)
					 (debug/default-trace-exit-handler name expr args results))))

; Invariant: a procedure is on each of these lists at most once.

(define *traced* '())                   ; ((proc . wrap) ...)
(define *broken* '())                   ; ((proc . wrap) ...)

(define (initialize-trace-and-break)

  ; Install an evaluator that resets the trace level each time a top-level 
  ; expression is evaluated. (This isn't ideal, because anyone else may
  ; install their own evaluator, but it's OK for now.)

  (let ((old-evaluator (repl-evaluator)))
    (repl-evaluator (lambda (expr env)
                      (parameterize ((trace-level 0))
                        (old-evaluator expr env)))))

  ; FIXME: can do better here.  If offset=0 then we may be able to use
  ;        procedure-arity to extract the arguments from the system stack
  ;        frame.  On the other hand, the debugger can do that too.

  (break-handler
   (lambda (proc offset)
     (if (debug/breakpoints-enable)
         (debug/signal-breakpoint proc #f)))))

(define (trace p)
  (debug/trace-it p #t #t))

(define (trace-entry p)
  (debug/trace-it p #t #f))

(define (trace-exit p)
  (debug/trace-it p #f #t))

(define (debug/trace-it p enter? exit?)
  (call-without-interrupts
    (lambda ()
      (cond ((assv p *traced*)
             (untrace p)
             (debug/trace-it p enter? exit?))
            ((not (procedure? p))
             (debug/displayln "TRACE: ignoring non-procedure " p))
            (else
             (let* ((name 
                     (or (procedure-name p) "anonymous"))
                    (expr 
                     (procedure-expression p))
                    (wrap-info
                     (debug/wrap-procedure
                      p
                      (lambda (compute)
                        (lambda args

                          (define (trace-both)
                            (parameterize ((debug/breakpoints-enable #f))
                              (debug/trace-enter-msg name expr args))
                            (let-values ((results
                                          (parameterize ((trace-level 
                                                          (+ (trace-level) 1)))
                                            (apply compute args))))
                              (parameterize ((debug/breakpoints-enable #f))
                                (debug/trace-exit-msg name expr args results))
                              (apply values results)))

                          (define (trace-entry)
                            (parameterize ((debug/breakpoints-enable #f))
                              (debug/trace-enter-msg name expr args))
                            (apply compute args))
                      
                          (define (trace-exit)
                            (let-values ((results (apply compute args)))
                              (parameterize ((debug/breakpoints-enable #f))
                                (debug/trace-exit-msg name expr args results))
                              (apply values results)))

                          (if (debug/breakpoints-enable)
                              (cond ((and enter? exit?) (trace-both))
                                    (enter?             (trace-entry))
                                    (exit?              (trace-exit))
                                    (else               ???))
                              (apply compute args)))))))
               (set! *traced* (cons (cons p wrap-info) *traced*))
               p))))))

(define (untrace . rest)
  (call-without-interrupts
    (lambda ()
      (cond ((null? rest)
             (for-each untrace (map car *traced*)))
            ((null? (cdr rest))
             (let ((probe (assv (car rest) *traced*)))
               (if probe
                   (begin (debug/undo-wrapping (cdr probe))
                          (set! *traced* (remq! probe *traced*))))))
            (else
             (for-each untrace rest)))))
  (unspecified))

(define (break-entry p)
  (call-without-interrupts
    (lambda ()
      (cond ((not (procedure? p))
             (error p " is not a procedure."))
            ((assv p *broken*) p)
            (else
             (let ((wrap-info
                    (debug/wrap-procedure
                     p
                     (lambda (compute)
                       (lambda args
                         (debug/signal-breakpoint p args)
                         (apply compute args))))))
               (set! *broken* (cons (cons p wrap-info) *broken*))
               p))))))

(define (unbreak . rest)
  (call-without-interrupts
    (lambda ()
      (cond ((null? rest)
             (for-each unbreak (map car *broken*)))
            ((null? (cdr rest))
             (let ((probe (assv (car rest) *broken*)))
               (if probe
                   (begin (debug/undo-wrapping (cdr probe))
                          (set! *broken* (remq! probe *broken*))))))
            (else
             (for-each unbreak rest)))))
  (unspecified))

(define (debug/signal-breakpoint p args)  ; args is a list or #f
  (cond ((and p args)
         (debug/display "Breakpoint: ")
         (debug/trace-enter-msg (or (procedure-name p) "anonymous")
                                (procedure-expression p)
                                args)
         (debug/enter-debugger #t))
        (else
         (if (procedure-name p)
             (debug/displayln "Breakpoint in " (procedure-name p))
             (debug/displayln "Breakpoint."))
         (debug/enter-debugger #t))))

(define (debug/trace-print x)
  (debug/print-object x))

(define (debug/trace-enter-msg name expr args)
  ((trace-entry-handler) name expr args))

(define (debug/default-trace-entry-handler name expr args)
  (debug/display (make-string (trace-level) #\space) "Enter " name)
  (debug/display-args (if expr (cadr expr) #f) args)
  (debug/displayln))

; If we know the formals, print a list on the form (a1=v1 a2=v2 ...)
; If we don't know the formals, print a list with each value on a separate
;  line, indented, except when only one argument, which is printed
;  on the same 

(define (debug/display-args formals args)
  (debug/display "(")
  (let ((indent (make-string (trace-level) #\space)))
    (if formals
        (let loop ((formals formals) (args args))
          (cond ((null? formals))
                ((pair? formals)
                 (debug/display (car formals) "=")
                 (debug/trace-print (car args))
                 (if (not (null? (cdr formals)))
                     (debug/display " "))
                 (loop (cdr formals) (cdr args)))
                (else
                 (debug/display formals "=")
                 (debug/trace-print args))))
        (cond ((null? args))
              ((null? (cdr args))
               (debug/trace-print (car args)))
              (else
               (debug/displayln)
               (let loop ((args args))
                 (debug/display indent "    ")
                 (debug/trace-print (car args))
                 (if (not (null? (cdr args)))
                     (begin
                       (debug/displayln)
                       (loop (cdr args)))
                     (debug/display indent)))))))
  (debug/display ")"))

(define (debug/trace-exit-msg name exp args results)
  ((trace-exit-handler) name exp args results))

(define (debug/default-trace-exit-handler name exp args results)
  (debug/display (make-string (trace-level) #\space) "Leave " name)
  (debug/display-args (if exp (cadr exp) #f) args)
  (debug/display " => ")
  (cond ((null? results)
         (debug/display "[no values]"))
        ((null? (cdr results))
         (debug/trace-print (car results)))
        (else
         (debug/display "[" (length results) " values: ")
         (debug/trace-print results)
         (debug/display "]")))
  (debug/displayln))
  
; Installing and removing a debugging trap.
;
; There is more than one way to skin this cat.  The approach taken 
; here is portable across architectures, given the simple MAKE-TRAMPOLINE 
; primitive (whose implementation in MAL is also portable).
;
; Given a procedure PROC, we wish to intercept calls to PROC and divert
; them to another procedure.
;
; PROC is an arbitrary procedure. WRAPPER-CREATOR is a procedure of one
; argument, a procedure.  WRAPPER-CREATOR is called with a procedure
; that represents the computation of PROC; WRAPPER-CREATOR should return 
; a procedure that is compatible with the interface of PROC.  When PROC 
; is called, the call will be diverted to the procedure returned by 
; WRAPPER-CREATOR.
;
; DEBUG/WRAP-PROCEDURE returns a data structure that records the wrapping;
; it can be passed to DEBUG/UNDO-WRAPPING to undo the wrapping.

(define (debug/wrap-procedure proc wrapper-creator)
  (let* ((np         (procedure-copy proc))
         (trampoline (make-trampoline (wrapper-creator np)))
         (l          (procedure-length trampoline))
         (doc        (vector-ref (procedure-ref proc 1) 0)))
    (do ((i 0 (+ i 1)))
        ((= i l))
      (procedure-set! proc i (procedure-ref trampoline i)))
    (vector-set! (procedure-ref proc 1) 0 doc)
    (make-wrap-info proc np)))

(define (debug/undo-wrapping wrap-info)
  (let ((proc (wrap-info-proc wrap-info))
        (np   (wrap-info-newproc wrap-info)))
    (let ((l (procedure-length proc)))
      (do ((i 0 (+ i 1)))
          ((= i l) proc)
        (procedure-set! proc i (procedure-ref np i))))))

(define (make-wrap-info a b) (vector 'wrap-info a b))
(define (wrap-info-proc x) (vector-ref x 1))
(define (wrap-info-newproc x) (vector-ref x 2))

; eof
