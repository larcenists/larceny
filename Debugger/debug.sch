; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Bare-bones debugger (very experimental).
;
; Usage: when an error has occurred in the program, type (backtrace)
; or (debug) at the prompt.
;
; FIXME: When using nested debuggers, and using the Q command to pop out
;        of one level to the next level, the mysterious message "Procedure 
;        call caused a reset." is printed, hardly what the user would expect.
;
; FIXME: When using nested debuggers, there is a difference between
;        popping out one level and popping out all the way to the top level;
;        currently the latter is not possible.
;
; FIXME: Debug/print-object must be able to deal with structures; right now
;        it only prints #<WEIRD>, which is useless.

'(require 'pretty-print)
'(require 'inspect-cont)

(define debug/return #f)                ; Thunk that returns
(define debug/reset #f)                 ; Thunk that resets

(define *debug-print-length* 10)
(define *debug-level* 0)

(define (install-debugger)

  ; Install an error handler that invokes the debugger.

  (error-handler 
   (lambda the-error
     (with-console-i/o
      (lambda ()
        (newline)
        (decode-error the-error)
        (debug/enter-debugger #f)))))

  ; Install a keyboard interrupt handler that invokes the debugger so
  ; that keyboard interrupts can be handled and the computation can also
  ; be continued.

  (keyboard-interrupt-handler
   (lambda ()
     (let ((enabled? (disable-interrupts)))
       (with-console-i/o
        (lambda ()
          (display "Keyboard interrupt.") 
          (newline)
          (debug/enter-debugger #t)
          (if enabled? 
              (enable-interrupts (standard-timeslice))))))))

  ; Initialize the trace and breakpoint package.

  (initialize-trace-and-break)

  #t)

; This is at least roughly right.  Divert current-input-port and 
; current-output-port to the console in the dynamic extent of thunk.

(define (with-console-i/o thunk)
  (let ((conin  (current-input-port))
        (conout (current-output-port)))
    (dynamic-wind
     (lambda ()
       (reestablish-console))
     thunk
     (lambda ()
       (current-input-port conin)
       (current-output-port conout)))))

(define (debug)
  (format #t "Entering debugger; ? for help.~%")
  (let ((e (error-continuation)))
    (if (not e)
	(begin (display "No error continuation!")
	       (newline))
	(debug-continuation-structure #f e))))

(define (debug/enter-debugger continuable?)
  (display "Entering debugger; type \"?\" for help.")
  (newline)
  (debug-continuation-structure continuable? (current-continuation-structure)))

(define (backtrace)
  (debug/backtrace 0 (make-continuation-inspector (error-continuation))))

(define (debug-continuation-structure continuable? c . rest)
  (let ((inspector (make-continuation-inspector c))
	(display? (and (not (null? rest)) (car rest))))

    (define (user-input)
      (reestablish-console)
      (display "debug")
      (display (make-string *debug-level* #\>))
      (display " ")
      (flush-output-port)
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
		  (display "Bad command.  ? for help.")
		  (newline)
		  (loop #f)))))))

    (define (command-loop display-frame?)
      (let* ((reset-token (list 'reset-token))
             (result (debug/safely 
                      (lambda () (loop display-frame?))
                      reset-token)))
        (if (eq? reset-token result)
            (begin (newline)
                   (command-loop #f))
            result)))

    (define (inspect-continuation display-frame?)
      (let ((result (command-loop display-frame?)))
        (cond ((eq? result 'reset) 'reset)
              (continuable?        'return)
              (else
               (display "Computation is not continuable.")
               (newline)
               (inspect-continuation #f)))))

    (dynamic-wind
     (lambda () (set! *debug-level* (+ *debug-level* 1)))
     (lambda ()
       (case (call-with-current-continuation
              (lambda (k)
                (set! debug/return (lambda () (k 'return)))
                (set! debug/reset  (lambda () (k 'reset)))
                (inspect-continuation display?)))
         ((return) #t)
         ((reset) (reset))
         (else ???)))
     (lambda () (set! *debug-level* (- *debug-level* 1))))))


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
      thunk))))


; Debugger user interface
; Return the next token from the input source, or reset if EOF.

(define (debug/get-token)
  (let ((t (read)))
    (if (eof-object? t)
	(begin (display t)
               (newline)
               (reestablish-console)
               (debug/reset))
	t)))

; Safely prints a potentially circular object.

(define (debug/print-object obj)

  (define limitation *debug-print-length*)

  (define printed-elipsis #f)

  (define (display-limited obj)
    (cond ((<= limitation 0) 
	   (if (not printed-elipsis)
	       (begin (display "...")
		      (set! printed-elipsis #t))))
	  ((or (symbol? obj)
	       (boolean? obj)
	       (char? obj)
	       (number? obj)
	       (string? obj)
	       (procedure? obj)
	       (eq? (unspecified) obj)
	       (eq? (undefined) obj)
	       (eof-object? obj)
	       (null? obj)
	       (bytevector? obj)
	       (port? obj))
	   (write obj)
	   (set! limitation (- limitation 1)))
	  ((and (list? obj) (eq? (car obj) 'quote) (null? (cddr obj)))
	   (display #\')
	   (display-limited (cadr obj)))
	  ((pair? obj)
	   (set! limitation (- limitation 1))
	   (display "(")
	   (let loop ((obj obj))
	     (cond ((and (pair? obj) 
			 (not (or (<= limitation 0) (null? obj))))
		    (display-limited (car obj))
		    (if (not (null? (cdr obj)))
			(display " "))
		    (loop (cdr obj)))
		   ((null? obj))
		   ((and (not (pair? obj))
			 (not (<= limitation 0)))
		    (display ". ")
		    (display-limited obj))
		   ((not (null? obj))
		    (display "..."))))
	   (display ")")
	   (set! printed-elipsis #f))
	  ((vector? obj)
	   (display "#")
	   (display-limited (vector->list obj)))
	  (else
	   (display "#<WEIRD>")
	   (set! limitation (- limitation 1)))))

  (display-limited obj)
  (unspecified))

; Prints code.

(define (debug/print-code expr)
  (pretty-print expr))


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
  (display *inspector-help*))

(define (debug/down count inspector)
  (cond ((zero? count))
	((not (inspector 'down))
	 (format #t "Already at the bottom.~%"))
	(else
	 (debug/down (- count 1) inspector))))

(define (debug/up count inspector)
  (cond ((zero? count))
	((not (inspector 'up))
	 (format #t "Already at the top.~%"))
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
       (format #t "system continuation"))
      ((interpreted-primitive)
       (format #t "interpreted primitive ~a" (procedure-name proc)))
      ((interpreted-expression)
       (format #t "interpreted expression ")
       (debug/print-object expr))
      ((compiled-procedure)
       (format #t "compiled procedure ~a" (procedure-name proc)))
      (else
       (error "debug/summarize-frame: Unknown class " class)))
    (newline)))

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
      (display "#")
      (display i)
      (display ": ")
      (debug/print-object (f 'ref-slot i))
      (newline))))

(define (debug/code count inspector)
  (let ((expr (((inspector 'get) 'code) 'source-code)))
    (if expr
	(debug/print-code expr)
	(format #t "No code.~%"))))

(define (debug/inspect count inspector)

  (define (inspect-procedure proc)
    (cond ((eq? proc 0)
	   (format #t "Can't inspect a system procedure.~%"))
	  ((interpreted-expression? proc)
	   (debug/print-code (procedure-expression proc)))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i (procedure-length proc)))
	     (debug/print-object (procedure-ref proc i))
	     (newline)))))

  (let ((n (debug/get-token)))
    (cond ((eq? n '@)
	   (inspect-procedure (((inspector 'get) 'code) 'procedure)))
	  (else
	   (let ((obj ((inspector 'get) 'ref-slot n)))
	     (if (not (procedure? obj))
		 (format #t "Slot ~a does not contain a procedure.~%" n)
		 (inspect-procedure obj)))))))

; This parameter is set to #f whenever the debugger recursively calls
; EVAL and can be used by other code (see e.g. trace.sch) to determine
; whether breakpoints should be honored or not.

(define debug/breakpoints-enable 
  (let ((enable #t))
    (lambda rest
      (cond ((null? rest) enable)
            ((null? (cdr rest))
             (set! enable (car rest))
             enable)
            (else
             (error "Wrong number of arguments to debug/breakpoints-enable "
                    rest))))))
      
(define (debug/evaluate count inspector)

  (define (valid-frame-slot? frame n)
    (<= 0 n (- (frame 'slots) 1)))

  (define (evaluate args expr frame)
    (let* ((token (list 'token))
	   (proc  (debug/safely
		   (lambda ()
                     (let ((breakpt (debug/breakpoints-enable)))
                       (dynamic-wind
                        (lambda ()
                          (debug/breakpoints-enable #f))
                        (lambda ()
                          (eval expr (interaction-environment)))
                        (lambda ()
                          (debug/breakpoints-enable breakpt)))))
		   token)))
      (cond ((eq? proc token)
	     (format #t "Expression caused a reset.~%"))
	    ((not (procedure? proc))
	     (format #t "~a does not evaluate to a procedure: ~a" expr proc))
	    ((not (every? (lambda (n)
			    (valid-frame-slot? frame n))
			  args))
	     (format #t "Some frame slots are not valid in ~a" args))
	    (else
	     (let* ((actuals (map (lambda (n)
				    (frame 'ref-slot n))
				  args))
		    (res (debug/safely
			  (lambda () (apply proc actuals))
			  token)))
	       (cond ((eq? res token)
		      (format #t "Procedure call caused a reset.~%"))
		     ((eq? res (unspecified))
		      ; FIXME: really a conditional newline.
		      (newline))
		     (else
		      (debug/print-object res)
		      (newline))))))))

  (let* ((n    (debug/get-token))
	 (expr (debug/get-token)))
    (evaluate (if (pair? n) n (list n)) expr (inspector 'get))))

(define debug/command-list
  `((? . ,debug/help)
    (b . ,debug/backtrace)
    (c . ,debug/code)
    (d . ,debug/down)
    (e . ,debug/evaluate)
    (i . ,debug/inspect)
    (q . reset)
    (r . done)
    (s . ,debug/summarize-frame)
    (u . ,debug/up)
    (x . ,debug/examine-frame)
    ))

(define *inspector-help* "
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

; eof
