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
;
; (break-here)                  Break at this point
; (break-entry <proc>)          Break on entry to <proc>
; (unbreak <proc> ...)          Disable breaking for <procs>
; (unbreak)                     Disable breaking for all broken <procs>

; Requires:
;  (debug/print-object obj)     Print the object safely
;  (debug/enter-debugger cont?) Enter debugger in current context

; FIXME: BREAK-HERE should be called BREAK, but that name is currently
;        used by the primitive that should be called DEBUGVSM.
;
; FIXME: Come up with a better solution for resetting the trace level than
;        the redefinition of repl-evaluator.  The most obvious solution is
;        a set of REPL hooks: REPL-BEFORE-EVAL-HOOK and REPL-AFTER-EVAL-HOOK.
;        A more subtle solution may use dynamic-wind.
;
; FIXME: It may be useful to allow TRACE, TRACE-ENTRY, TRACE-EXIT, BREAK-ENTRY
;        to accept an optional name by which to identify the traced
;        procedure, since anonymous procedures are otherwise hard to
;        tell apart (other than by their code).

(define *trace-level* 0)
(define *traced* '())
(define *broken* '())
(define *breakpoints-enabled* #t)

; Cruft.

; Install an evaluator that resets the trace level each time a top-level 
; expression is evaluated. (This isn't ideal but it's OK.)

(let ((old-evaluator (repl-evaluator)))
  (repl-evaluator (lambda (expr env)
                    (set! *trace-level* 0)
                    (old-evaluator expr env))))


(define (trace p)       (debug/trace-it p #t #t))
(define (trace-entry p) (debug/trace-it p #t #f))
(define (trace-exit p)  (debug/trace-it p #f #t))

(define (debug/trace-it p enter? exit?)
  (cond ((assq p *traced*)
         p)
        ((not (procedure? p))
         (error "Cannot trace non-procedure " p))
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
                      (cond ((and enter? exit?)
                             (debug/trace-enter-msg name expr args)
                             (call-with-values
                               (lambda ()
                                 (set! *trace-level* (+ *trace-level* 1))
                                 (apply compute args))
                               (lambda results
                                 (set! *trace-level* (- *trace-level* 1))
                                 (debug/trace-exit-msg name expr args results)
                                 (apply values results))))
                            (enter?
                             (debug/trace-enter-msg name expr args)
                             (apply compute args))
                            (exit?
                             (call-with-values
                               (lambda () (apply compute args))
                               (lambda results
                                 (debug/trace-exit-msg name expr args results)
                                 (apply values results))))
                            (else
                             ???)))))))
           (set! *traced* (cons (cons p wrap-info) *traced*))
           p))))

(define (untrace . rest)
  (cond ((null? rest)
         (for-each untrace (map car *traced*)))
        ((null? (cdr rest))
         (let ((p (car rest)))
           (let ((probe (assq p *traced*)))
             (if probe
                 (begin (debug/undo-wrapping (cdr probe))
                        (set! *traced* (remq! probe *traced*))))
             p)))
        (else
         (for-each untrace rest)))
  (unspecified))

(define (break-here)
  (debug/breakpoint #f #f))

(define (break-entry p)
  (cond ((not (procedure? p))
         (error p " is not a procedure."))
        ((assv p *broken*)
         p)
        (else
         (let ((wrap-info
                (debug/wrap-procedure
                 p
                 (lambda (compute)
                   (lambda args
                     (debug/breakpoint p args)
                     (apply compute args))))))
           (set! *broken* (cons (cons p wrap-info) *broken*))
           p))))

(define (unbreak . rest)
  (cond ((null? rest)
         (for-each unbreak (map car *broken*)))
        ((null? (cdr rest))
         (let ((p (car rest)))
           (let ((probe (assq p *broken*)))
             (if probe
                 (begin (debug/undo-wrapping (cdr probe))
                        (set! *broken* (remq! probe *broken*))))
             p)))
        (else
         (for-each unbreak rest)))
  (unspecified))

(define (debug/breakpoint p args)  ; args is a list or #f
  (cond ((not *breakpoints-enabled*))
        ((and p args)
         (display "Breakpoint: ")
         (debug/trace-enter-msg (or (procedure-name p) "anonymous")
                                (procedure-expression p)
                                args)
         (debug/enter-debugger #t))
        (else
         (display "Breakpoint.")
         (newline)
         (debug/enter-debugger #t))))

(define (debug/call-with-breakpoints-disabled thunk)
  (let ((outside *breakpoints-enabled*))
    (dynamic-wind
     (lambda ()
       (set! *breakpoints-enabled* #f))
     thunk
     (lambda ()
       (set! *breakpoints-enabled* outside)))))

(define (debug/trace-print x)
  (debug/print-object x))

(define (debug/trace-enter-msg name expr args)
  (display (make-string *trace-level* #\space))
  (display "Enter ")
  (display name)
  (debug/display-args (if expr (cadr expr) #f) args)
  (newline))

; If we know the formals, print a list on the form (a1=v1 a2=v2 ...)
; If we don't know the formals, print a list with each value on a separate
;  line, indented, except when only one argument, which is printed
;  on the same 

(define (debug/display-args formals args)
  (display "(")
  (let ((indent (make-string *trace-level* #\space)))
    (if formals
        (let loop ((formals formals) (args args))
          (cond ((null? formals))
                ((pair? formals)
                 (display (car formals))
                 (display "=")
                 (debug/trace-print (car args))
                 (if (not (null? (cdr formals)))
                     (display " "))
                 (loop (cdr formals) (cdr args)))
                (else
                 (display formals)
                 (display "=")
                 (debug/trace-print args))))
        (cond ((null? args))
              ((null? (cdr args))
               (debug/trace-print (car args)))
              (else
               (newline)
               (let loop ((args args))
                 (display indent)
                 (display "    ")
                 (debug/trace-print (car args))
                 (if (not (null? (cdr args)))
                     (begin
                       (newline)
                       (loop (cdr args)))
                     (display indent)))))))
  (display ")"))

(define (debug/trace-exit-msg name exp args results)
  (display (make-string *trace-level* #\space))
  (display "Leave ")
  (display name)
  (debug/display-args (if exp (cadr exp) #f) args)
  (display " => ")
  (cond ((null? results)
         (display "[no values]"))
        ((null? (cdr results))
         (debug/trace-print (car results)))
        (else
         (display "[")
         (display (length results))
         (display " values: ")
         (debug/trace-print results)
         (display "]")))
  (newline))
  
; Installing and removing a debugging trap.
;
; There is more than one way to skin this cat.  The approach taken 
; here is portable across architectures, given the simple MAKE-TRAMPOLINE 
; primitive (whose implementation in MAL is also portable).
;
; PROC is an arbitrary procedure.  WRAPPER-CREATOR is a procedure of one
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
