; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Bare-bones debugger.
;
; The debugger will be invoked automatically when an error is encountered
; or when a keyboard interrupt is signalled.  After the user has left the
; debugger, debugging can be continued by evaluating (debug).
;
; FIXME: When stopped in a breakpoint, or at an error, it really 
;        should set the current frame to the proc where the breakpt
;        occurred, not past the system-continuation/enter-debugger
;        thing.
;
; FIXME: The E command must change: it should take an expression to be
;        evaluated in the environment of the current frame, and print
;        the results.


'(begin (load "Debugger/inspect-cont.sch")
        (load "Debugger/trace.sch"))

(define debug/reset #f)                 ; Dynamically bound: thunk that resets
(define debug-level 0)                  ; Dynamically bound

(define *debug-print-length* 7)         ; For PRINT-LENGTH
(define *debug-print-level* 7)          ; for PRINT-LEVEL

; The user can restart the debugger by typing (debug); the error
; continuation is saved by the error handler.

(define (debug)
  (let ((e (error-continuation)))
    (if (not e)
        (debug/displayln "No error continuation!")
        (begin (debug/displayln "Entering debugger; ? for help.")
               (debug-continuation-structure #f e)))))

(define error-continuation
  (make-parameter "error-continuation" #f))


(define (install-debugger)

  ; Install an error handler that invokes the debugger.

  (error-handler 
   (lambda the-error
     (error-continuation (current-continuation-structure))
     (debug/displayln)
     (parameterize ((print-length *debug-print-length*)
                    (print-level *debug-print-level*))
       (decode-error the-error (console-output-port)))
     (debug/enter-debugger #f)))

  ; Install a keyboard interrupt handler that invokes the debugger so
  ; that keyboard interrupts can be handled and the computation can also
  ; be continued.

  (keyboard-interrupt-handler
   (lambda ()
     (let ((enabled? (disable-interrupts)))
       (debug/displayln "Keyboard interrupt.") 
       (debug/enter-debugger #t)
       (if enabled? 
           (enable-interrupts (standard-timeslice))))))

  ; Initialize the trace and breakpoint package.

  (initialize-trace-and-break)

  #t)

(define (debug/enter-debugger continuable?)
  (debug/displayln "Entering debugger; type \"?\" for help.")
  (debug-continuation-structure continuable? 
                                (current-continuation-structure)))

(define (debug-continuation-structure continuable? c . rest)
  (let ((inspector (make-continuation-inspector c))
	(display? (and (not (null? rest)) (car rest))))

    (define (user-input)
      (debug/display "debug" (make-string debug-level #\>) " ")
      (let* ((x     (debug/get-token))
             (count (if (number? x) x 1))
             (cmd   (if (number? x) (debug/get-token) x)))
        (values count (debug-command cmd))))

    (define (loop display-frame?)
      (let ((current (inspector 'get)))
	(if display-frame?
	    (debug/summarize-frame 0 inspector))
	(call-with-values
	 user-input
	 (lambda (count cmd)
	   (cond ((eq? cmd 'done)  'done)
                 ((eq? cmd 'reset) 'reset)
		 (cmd
		  (cmd count inspector)
		  (loop (not (current 'same? (inspector 'get)))))
		 (else
		  (debug/displayln "Bad command.  ? for help.")
		  (loop #f)))))))

    (define (command-loop display-frame?)
      (let* ((reset-token (list 'reset-token))
             (result (debug/safely 
                      (lambda () (loop display-frame?))
                      reset-token)))
        (if (eq? reset-token result)
            (begin (debug/displayln)
                   (command-loop #f))
            result)))

    (define (inspect-continuation display-frame?)
      (let ((result (command-loop display-frame?)))
        (cond ((eq? result 'reset) 'reset)
              (continuable?        'return)
              (else
               (debug/displayln "Computation is not continuable.")
               (inspect-continuation #f)))))

    (case (call-with-current-continuation
           (lambda (k)
             ; FIXME: really fluid-let
             (let ((old-reset debug/reset)
                   (old-level debug-level))
               (dynamic-wind
                (lambda ()
                  (set! debug/reset (lambda () (k 'reset)))
                  (set! debug-level (+ debug-level 1)))
                (lambda ()
                  (inspect-continuation display?))
                (lambda ()
                  (set! debug/reset old-reset)
                  (set! debug-level old-level))))))
      ((return) #t)
      ((reset) (reset))
      (else ???))))


; Safe evaluation
;
; Catch a reset, but don't save the new error continuation.
; It would also be possible to treat resets like a kind of exception.

(define (debug/safely thunk reset-token)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
	(k reset-token))
      (lambda ()
        (call-with-error-handler
         (lambda error
           (decode-error error (console-output-port))
           (k (unspecified)))
         thunk))))))


; Debugger user interface.
; Return the next token from the input source, or reset if EOF.

(define (debug/get-token)
  (let ((t (debug/read)))
    (if (eof-object? t)
        (begin (debug/displayln t)
               (debug/reset))
        t)))


; Debugger commands
;
; Each command takes a count (optional; defaults to 1) and an inspector,
; and returns nothing.

; Given a symbol, return the command procedure or #f.

(define (debug-command x)
  (let ((probe (assq x debug/command-list)))
    (if probe
	(cdr probe)
	#f)))

(define (debug/help count inspector)
  (debug/display *inspector-help*))

(define (debug/repl count inspector)
  (with-input-from-port (console-input-port)
    (lambda ()
      (with-output-to-port (console-output-port)
        repl))))

(define (debug/down count inspector)
  (cond ((zero? count))
	((not (inspector 'down))
	 (debug/displayln "Already at the bottom."))
	(else
	 (debug/down (- count 1) inspector))))

(define (debug/up count inspector)
  (cond ((zero? count))
	((not (inspector 'up))
         (debug/displayln "Already at the top."))
	(else
	 (debug/up (- count 1) inspector))))

(define (debug/summarize-frame count inspector . prefix)
  (let* ((frame (inspector 'get))
	 (code  (frame 'code))
	 (class (code 'class))
	 (expr  (code 'expression))
	 (proc  (code 'procedure)))
    (if (not (null? prefix))
	(display (car prefix)))
    (case class
      ((system-procedure)
       (debug/display "system continuation"))
      ((interpreted-primitive)
       (debug/display "interpreted primitive " (procedure-name proc)))
      ((interpreted-expression)
       (debug/display "interpreted expression ")
       (debug/print-object expr))
      ((compiled-procedure)
       (debug/display "compiled procedure " (procedure-name proc)))
      (else
       (error "debug/summarize-frame: Unknown class " class)))
    (debug/displayln)))

(define (debug/abort count inspector)
  (exit 1))

(define (debug/backtrace count inspector)
  
  (define (loop c)
    (let ((f (c 'get)))
      (if (f 'same? (inspector 'get))
	  (debug/summarize-frame 0 c "=> ")
	  (debug/summarize-frame 0 c "   ")))
    (if (c 'down)
	(loop c)))

  (loop (inspector 'clone)))

(define (debug/examine-frame count inspector)
  (let* ((f (inspector 'get))
	 (code (f 'code))
	 (expr (code 'expression)))
    (if expr
	(pretty-print expr))
    (do ((i 0 (+ i 1)))
	((= i (f 'slots)))
      (debug/display "#" i ": ")
      (debug/print-object (f 'ref-slot i))
      (debug/displayln))))

(define (debug/code count inspector)
  (let ((expr (((inspector 'get) 'code) 'source-code)))
    (if expr
	(debug/print-code expr)
	(debug/displayln "No code."))))

(define (debug/inspect count inspector)

  (define (inspect-procedure proc)
    (cond ((eq? proc 0)
	   (debug/displayln "Can't inspect a system procedure."))
	  ((interpreted-expression? proc)
	   (debug/print-code (procedure-expression proc)))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i (procedure-length proc)))
	     (debug/print-object (procedure-ref proc i))
	     (debug/displayln)))))

  (let ((n (debug/get-token)))
    (cond ((eq? n '@)
	   (inspect-procedure (((inspector 'get) 'code) 'procedure)))
	  (else
	   (let ((obj ((inspector 'get) 'ref-slot n)))
	     (if (not (procedure? obj))
		 (debug/displayln "Slot " n " does not contain a procedure.")
		 (inspect-procedure obj)))))))

; This parameter is set to #f whenever the debugger recursively calls
; EVAL and can be used by other code (see e.g. trace.sch) to determine
; whether breakpoints should be honored or not.

(define debug/breakpoints-enable 
  (make-parameter "debug/breakpoints-enable" #t))

(define (debug/call-with-breakpoints-disabled thunk)
  (parameterize ((debug/breakpoints-enable #f))
    (thunk)))
  
(define (debug/evaluate count inspector)

  (define (valid-frame-slot? frame n)
    (<= 0 n (- (frame 'slots) 1)))

  (define (evaluate args expr frame)
    (let* ((token (list 'token))
	   (proc  (debug/safely
		   (lambda ()
                     (parameterize ((debug/breakpoints-enable #f))
                       (eval expr (interaction-environment))))
		   token)))
      (cond ((eq? proc token)
	     (debug/displayln "Expression caused a reset."))
	    ((not (procedure? proc))
	     (debug/displayln expr " does not evaluate to a procedure: " proc))
	    ((not (every? (lambda (n)
			    (valid-frame-slot? frame n))
			  args))
	     (debug/displayln "Some frame slots are not valid in " args))
	    (else
	     (let* ((actuals (map (lambda (n)
				    (frame 'ref-slot n))
				  args))
		    (res (debug/safely
			  (lambda () (apply proc actuals))
			  token)))
	       (cond ((eq? res token)
		      (debug/displayln "Procedure call caused a reset."))
		     ((eq? res (unspecified))
		      ; FIXME: really a conditional newline.
		      (debug/displayln))
		     (else
		      (debug/print-object res)
		      (debug/displayln))))))))

  (let* ((n    (debug/get-token))
	 (expr (debug/get-token)))
    (evaluate (if (pair? n) n (list n)) expr (inspector 'get))))

(define debug/command-list
  `((? . ,debug/help)
    (a . ,debug/abort)
    (b . ,debug/backtrace)
    (c . ,debug/code)
    (d . ,debug/down)
    (e . ,debug/evaluate)
    (i . ,debug/inspect)
    (n . ,debug/repl)
    (q . reset)
    (r . done)
    (s . ,debug/summarize-frame)
    (u . ,debug/up)
    (x . ,debug/examine-frame)
    ))

(define *inspector-help* "
  A           Abort (exit from Larceny).
  B           Print backtrace of continuation.
  C           Print source code (if available).
  D           Down to previous activation record.
  E n expr    Expr is evaluated in the current interaction environment 
              and must evaluate to a procedure.   It is passed the contents
              of slot n from the current activation record, and the result, 
              if not unspecified, is printed.
  E (n1 ... nk) expr
              Expr is evaluated in the current interaction environment and
              must evaluate to a procedure.   It is passed the contents of
              slots n1 through nk from the current activation record, and 
              the result, if not unspecified, is printed.
  I n         Inspect the procedure in slot n of the current activation record.
  I @         Inspect the active procedure.
  N           Enter a nested REPL.
  Q           Quit the debugger and abort the computation.
  R           Return from the debugger and continue the computation.
  S           Summarize the contents of the current activation record.
  U           Up to the next activation record.
  X           Examine the contents of the current activation record.

The B, D, and U commands can be prefixed with a count, for example, 
`5 U' moves up five activation records, and `10 B' displays the next 
10 activation records.  The default for B is to display all the 
activations; the default count for D and U is 1.

")

(define (debug/display . xs)
  (for-each (lambda (x) 
              (display x (console-output-port))) 
            xs))

(define (debug/displayln . xs)
  (apply debug/display xs)
  (newline (console-output-port)))

(define (debug/read)
  (read (console-input-port)))

(define (debug/print-object obj)
  (parameterize ((print-length *debug-print-length*)
                 (print-level *debug-print-level*))
    (write obj (console-output-port))))

(define (debug/print-code expr)
  (parameterize ((print-length #f)
                 (print-level #f))
    (pretty-print expr (console-output-port))))

; eof
