; Experimental/debug.sch
; Larceny -- bare-bones ad-hock *prototype* debugger
;
; $Id$
;
; Usage: when an error has occurred in the program, type (backtrace)
; or (debug) at the prompt.
;
; Things we need to fix:
;  - display procedure-expression, if defined
;  - display parameter names, if known
;  - display arity, if known
;  - must work (even better) with interpreted code, although it's improving.

; Data structure offsets

(define frame:return-offset 0)
(define frame:dynamic-link 1)
(define frame:reg0 2)
(define frame:procedure frame:reg0)

(define procedure:code 0)
(define procedure:constants 1)
(define procedure:static-link 2)
(define procedure:reg0 3)

(define constants:documentation 0)

(define *debug-exit* #f)

; Accessors

(define (procedure-documentation proc)
  (if (evaluator-procedure? proc)
      (evaluator-procedure-documentation proc)
      (vector-ref (procedure-ref proc procedure:constants)
		  constants:documentation)))

(define (evaluator-procedure? proc)
  (let ((x (procedure-ref proc (- (procedure-length proc) 1))))
    (and (pair? x) (eq? (car x) '$evalproc))))

(define (evaluator-primitive? proc)
  (let ((x (procedure-ref proc (- (procedure-length proc) 1))))
    (and (pair? x) (eq? (car x) '$evalprim))))

(define (evaluator-primitive-name proc)
  (let ((x (procedure-ref proc (- (procedure-length proc) 1))))
    (cadr x)))

(define (evaluator-procedure-documentation proc)
  (let ((x (cdr (procedure-ref proc (- (procedure-length proc) 1)))))
    (vector "<expression>" x #f #f #f)))

(define (continuation-return-address frame)
  (* (vector-ref frame frame:return-offset) 4))

(define (continuation-length c)
  (let ((x 0))
    (walk-continuation
     c
     (lambda (f)
       (set! x (+ x 1))
       #t))
    x))

; Generic continuation walker

(define (walk-continuation chain p)
  (if (or (not chain)
	  (null? chain))
      (unspecified)
      (let ((res (p chain)))
	(if res
	    (walk-continuation (vector-ref chain frame:dynamic-link) p)))))

; Some useful examples

(define (backtrace)
  (walk-continuation
   (error-continuation)
   frame-summary))

(define (frame-summary frame . rest)

  (define noinfo "[no information available]")
  (define localinfo "local procedure ~a")
  (define procinfo "procedure ~a")

  (define (summarize-documentation doc)
    (cond ((vector? doc)
	   (format #t procinfo (vector-ref doc 0)))
	  ((list? doc)
	   (let ((retaddr (continuation-return-address frame)))
	     (let loop ((doc doc))
	       (cond ((null? doc)
		      (format #t noinfo))
		     ((and (>= retaddr (caar doc))
			   (null? (cdr doc)))
		      (format #t localinfo (vector-ref (cdar doc) 0)))
		     ((and (>= retaddr (caar doc))
			   (< retaddr (caadr doc)))
		      (format #t localinfo (vector-ref (cdar doc) 0)))
		     (else
		      (loop (cdr doc)))))))
	  (else
	   (format #t noinfo))))

  (let ((proc (vector-ref frame frame:procedure)))
    (if (not (null? rest))
	(display (car rest)))
    (cond ((eq? proc 0)
	   (format #t "system continuation"))
	  ((evaluator-procedure? proc)
	   (format #t "expression ")
	   (display-limited
	    (vector-ref (evaluator-procedure-documentation proc) 1)
	    *debug-print-length*))
	  ((evaluator-primitive? proc)
	   (format #t "primitive ~a" (evaluator-primitive-name proc)))
	  (else
	   (summarize-documentation (procedure-documentation proc))))
    (format #t "; ~a slots.~%" (count-saved-slots frame))
    #t))

(define (count-saved-slots frame)
  (- (vector-length frame) frame:reg0 1))

(define (frame-slots frame)
  (let* ((len (vector-length frame))
	 (v   (make-vector (- len frame:reg0 1))))
    (do ((i (+ frame:reg0 1) (+ i 1))
	 (j 0 (+ j 1)))
	((= i len) v)
      (vector-set! v j (vector-ref frame i)))))

(define (examine-frame frame)
  (let ((slots (frame-slots frame)))
    (do ((i 0 (+ i 1)))
	((= i (vector-length slots)))
      (display "#")
      (display i)
      (display ": ")
      (display-limited (vector-ref slots i) *debug-print-length*)
      (newline))))

(define (next-frame frame)
  (vector-ref frame frame:dynamic-link))

(define (valid-frame-slot? frame n)
  (and (fixnum? n)
       (< -1 n (count-saved-slots frame))))

(define (frame-ref frame slot)
  (vector-ref frame (+ slot frame:reg0 1)))

(define (frame-set! frame slot value)
  (vector-set! frame (+ slot frame:reg0 1) value))

(define (debug)
  (format #t "Entering debugger; ? for help.~%")
  (let ((e (error-continuation)))
    (if (not e)
	(begin (display "No error continuation!")
	       (newline))
	(debug-continuation-structure e))))

(define (debug-continuation-structure c)
  (call-with-current-continuation
   (lambda (k)
     (set! *debug-exit* k)
     (inspect-continuation c)))
  (set! *debug-exit* #f)
  (unspecified))

(define *debug-print-length* 7)

(define (inspect-continuation c)

  ; This catches the reset, but does not save the new error continuation.

  (define (safely thunk token)
    (call-with-current-continuation
     (lambda (k)
       (call-with-reset-handler
	(lambda ()
	  (k token))
	thunk))))

  (define (get-token)
    (let ((t (read)))
      (if (eof-object? t)
	  (begin (display t) (newline)
		 (*debug-exit* #f))
	  t)))

  (define (evaluate args expr frame token)
    (let* ((token (or token (list 'token)))
	   (proc  (safely
		   (lambda ()
		     (eval expr (interaction-environment)))
		   token)))
      (cond ((eq? proc token)
	     (format #t "Expression caused a reset.~%")
	     token)
	    ((not (procedure? proc))
	     (format #t "~a does not evaluate to a procedure: ~a" expr proc)
	     token)
	    ((not (every? (lambda (n)
			    (valid-frame-slot? frame n))
			  args))
	     (format #t "Some frame slots are not valid in ~a" args)
	     token)
	    (else
	     (let* ((actuals (map (lambda (n)
				    (frame-ref frame n))
				  args))
		    (res (safely
			  (lambda () (apply proc actuals))
			  token)))
	       (cond ((eq? res token)
		      (format #t "Procedure call caused a reset.~%")
		      token)
		     ((not (eq? res (unspecified)))
		      (display-limited res *debug-print-length*)
		      (newline)
		      res)
		     (else
		      res)))))))
	      
  (define (set-slot args expr res frame)
    (let* ((token (list 'token))
	   (value (evaluate args expr frame token)))
      (if (not (eq? value token))
	  (if (valid-frame-slot? frame res)
	      (frame-set! frame res value)
	      (format #t "Invalid result slot ~a~%" res)))))

  (define (cmd:help frame prev)
    (display inspector-help)
    (loop frame prev #f))

  (define (cmd:down count frame prev)
    (if (zero? count)
	(loop frame prev #t)
	(let ((next (next-frame frame)))
	  (if (not next)
	      (begin (display "Already at the top.")
		     (newline)
		     (loop frame prev #f))
	      (cmd:down (- count 1) next (cons frame prev))))))

  (define (cmd:up count frame prev)
    (if (zero? count)
	(loop frame prev #t)
	(if (null? prev)
	    (begin (display "Already at the bottom.")
		   (newline)
		   (loop frame prev #f))
	    (cmd:up (- count 1) (car prev) (cdr prev)))))

  (define (cmd:examine frame prev)
    (examine-frame frame)
    (loop frame prev #f))

  (define (cmd:backtrace count frame prev)
  
    (define (doit c)
      (walk-continuation 
       c 
       (lambda (f)
	 (if (eq? f frame)
	     (frame-summary f "=> ")
	     (frame-summary f "   "))
	 (if (number? count)
	     (begin (set! count (- count 1))
		    (> count 0))
	     #t))))

      (if count
	  (doit c)			; [sic!]
	  (let ((c-length (continuation-length c)))
	    (if (<= c-length 20)
		(doit c)
		(begin
		  (format #t "The continuation has ~a frames.~%~a~%"
			  c-length
			  "Are you sure you want to see them all? (Y/N) ")
		  (let ((x (get-token)))
		    (if (eq? x 'y)
			(doit c)))))))
      (loop frame prev #f))

  (define (cmd:summarize frame prev)
    (frame-summary frame)
    (loop frame prev #f))

  (define (cmd:code frame prev)
    (let ((p (vector-ref frame frame:procedure)))
      (if (procedure? p)
	  (let ((code (procedure-expression p)))
	    (if code
		(pretty-print code)
		(format #t "No code.~%")))
	  (format #t "No code.~%")))
    (loop frame prev #f))

  (define (cmd:evaluate frame prev)
    (let* ((n    (get-token))
	   (expr (get-token)))
      (evaluate (if (pair? n) n (list n)) expr frame #f)
      (loop frame prev #f)))

  (define (cmd:assign frame prev)
    (let* ((n    (get-token))
	   (expr (get-token))
	   (res  (get-token)))
      (set-slot (if (pair? n) n (list n)) expr res frame)
      (loop frame prev #f)))

  (define (inspect-procedure proc)
    (cond ((eq? proc 0)
	   (format #t "can't inspect a system procedure.~%"))
	  ((evaluator-procedure? proc)
	   (display-source
	    (vector-ref (evaluator-procedure-documentation proc) 1)))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i (procedure-length proc)))
	     (display-limited (procedure-ref proc i) *debug-print-length*)
	     (newline)))))

  (define (cmd:inspect frame prev)
    (let ((n (get-token)))
      (cond ((eq? n '@)
	     (inspect-procedure (vector-ref frame frame:procedure)))
	    ((not (valid-frame-slot? frame n))
	     (format #t "Not a valid argument: ~a~%" n))
	    (else
	     (let ((obj (frame-ref frame n)))
	       (if (not (procedure? obj))
		   (format #t "Slot ~a does not contain a procedure.~%" n)
		   (inspect-procedure obj)))))
      (loop frame prev #f)))

  (define (loop frame prev moved?)
    (if moved?
	(frame-summary frame))
    (display "Debug> ")
    (flush-output-port)
    (let ((x (get-token)))
      (let* ((count (if (number? x) x 1))
	     (cmd   (if (number? x) (get-token) x)))
	(cond ((eq? cmd '?)  (cmd:help frame prev))
	      ((eq? cmd 'd)  (cmd:down count frame prev))
	      ((eq? cmd 'u)  (cmd:up count frame prev))
	      ((eq? cmd 'x)  (cmd:examine frame prev))
	      ((eq? cmd 'q)  'done)
	      ((eq? cmd 'c)  (cmd:code frame prev))
	      ((eq? cmd 'b)  (cmd:backtrace (if (number? x) count #f)
					    frame prev))
	      ((eq? cmd 's)  (cmd:summarize frame prev))
	      ((eq? cmd '=>) (cmd:evaluate frame prev))
	      ((eq? cmd '!)  (cmd:assign frame prev))
	      ((eq? cmd 'i)  (cmd:inspect frame prev))
	      (else
	       (display "Bad command.  ? for help.")
	       (newline)
	       (loop frame prev #f))))))

  (let ((token (list 'token)))
    (let outer ((redisplay #t))
      (let ((res (safely 
		  (lambda ()
		    (loop c '() redisplay))
		  token)))
	(if (eq? res token)
	    (begin (newline)
		   (outer #f))))))
  (unspecified))

(define inspector-help "
b           Print backtrace of frames.
c           Show source code (if available).
d           Down a frame.
i n         Inspect the procedure in slot n of the current frame.
i @         Inspect the procedure in the current continuation frame.
q           Quit the debugger.
s           Summarize the current frame.
u           Up a frame.
x           Examine the current frame contents.
=> n expr   Expr is evaluated in the current interaction environment and
            must evaluate to a procedure.   It is passed the contents of
            slot n from the current frame, and the result, if not unspecified,
            is printed.  [Multiple values not yet supported.]
=> (n1 ... nk) expr
            Expr is evaluated in the current interaction environment and
            must evaluate to a procedure.   It is passed the contents of
            slots n1 through nk from the current frame, and the result, 
            if not unspecified, is printed.  [Multiple values not yet
            supported.]
! n expr m  Expr is evaluated in the current interaction environment and
            must evaluate to a procedure.   It is passed the contents of
            slot n from the current frame, and the result is stored in
            slot m in the current frame.  The result, if not unspecified,
            is printed.  [Multiple values not yet supported.]
! (n1 ... nk) expr m
            Expr is evaluated in the current interaction environment and
            must evaluate to a procedure.   It is passed the contents of
            slots n1 through nk from the current frame, and the result
            is stored in slot m in the current frame.  The result, if not
            unspecified, is printed.  [Multiple values not yet supported.]

The b, d, and u commands can be prefixed with a count; for example, 
`5 u' moves up five frames, and `10 b' displays the next 10 frames.
The default for b is to display all the frames; the default count for
d and u is 1.

")



; Safely prints a potentially circular object.

(define (display-limited obj limitation)

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
	   (display obj)
	   (set! limitation (- limitation 1)))
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


; Old procedures (not very useful any more).

(define (walk chain)
  (walk-continuation
   chain
   (lambda (frame)
     (let ((proc (vector-ref frame frame:procedure)))
       (if (or (not proc)
	       (eq? proc 0))
	   (begin (display "SYSTEM PROCEDURE")
		  (newline))
	   (begin
	     (display (procedure-documentation proc))
	     (display "   ")
	     (display (continuation-return-address frame))
	     (newline)))
       #t))))

(define (walk2 chain)
  (walk-continuation
   chain
   (lambda (frame)
     (cond ((vector? frame)
	    (let ((len (vector-length frame)))
	      (format #t "~%Frame length ~a~%" len)
	      (format #t "Return offset ~a~%" 
		      (continuation-return-address frame))
	      (let ((d (vector-ref frame frame:dynamic-link)))
		(cond ((vector? d)
		       (format #t "dynamic link is valid~%"))
		      ((not d)
		       (format #t "dynamic link is #f~%"))
		      (else
		       (format #t "dynamic link is invalid"))))
	      (let ((proc (vector-ref frame frame:procedure)))
		(format #t "procedure information: ~a~%"
			(cond ((eq? proc 0)
			       "system procedure")
			      (proc
			       (procedure-documentation proc))
			      (else
			       "(not a procedure)"))))
	      (do ((i (+ frame:reg0 1) (+ i 1)))
		  ((= i len))
		(format #t "#~a = ~a~%"
			(- i frame:reg0)
			(vector-ref frame i)))))
	   (else
	    (format #t "Invalid frame~%")))
     #t)))

(define (display-source source)
  (pretty-print source))

; Not currently used.
; This attempts to do some macro-unexpanding.  Pretty sick stuff.

(define (unexpand x)

  (define (unexpand-variable-name x)
    x)

  (define (unexpand-formals x)
    x)

  (define (if-expr? x) (and (pair? x) (eq? (car x) 'if)))
  (define (cond-expr? x) (and (pair? x) (eq? (car x) 'cond)))
  (define (let-expr? x) (and (pair? x) (eq? (car x) 'let)))

  (define (unexpand-if x)
    (let ((a (unexpand (cadr x)))
	  (b (unexpand (caddr x)))
	  (c (unexpand (cadddr x))))
      (cond ((if-expr? c)
	     `(cond (,a ,b)
		    (,(cadr c) ,(caddr c))
		    (else ,(cadddr c))))
	    ((cond-expr? c)
	     `(cond (,a ,b)
		    ,@(cdr c)))
	    (else
	     (list kwd:if a b c)))))

  (define (unexpand-lambda x)
    (cons 'lambda
	  (cons (unexpand-formals (cadr x))
		(map unexpand (cddr x)))))

  ; We should recognize internal definitions here.

  (define (unexpand-let x)
    `(let ,(map (lambda (name val)
		  (list (unexpand name)
			(unexpand val)))
		(cadr (car x))
		(cdr x))
       ,@(map unexpand (cddr (car x)))))

  (define (unexpand-begin x)
    (if (null? (cddr x))
	(unexpand (cadr x))
	(cons 'begin (map unexpand (cdr x)))))

  (cond ((pair? x)
	 (let ((op (car x)))
	   (cond ((eq? op kwd:if)
		  (unexpand-if x))
		 ((eq? op kwd:set!) 
		  (cons 'set! (map unexpand (cdr x))))
		 ((eq? op kwd:begin) 
		  (unexpand-begin x))
		 ((eq? op kwd:define) 
		  (list 'define (unexpand (cadr x)) (unexpand (caddr x))))
		 ((eq? op kwd:quote)
		  (list 'quote (cadr x)))
		 ((eq? op kwd:lambda)
		  (unexpand-lambda x))
		 ((and (pair? op) (eq? (car op) kwd:lambda))
		  (unexpand-let x))
		 (else
		  (map unexpand x)))))
	((symbol? x)
	 (unexpand-variable-name x))
	(else
	 x)))

; These must correspond to the keywords used by the macro expander for
; the primitives.

(define kwd:if 'if)
(define kwd:begin 'begin)
(define kwd:set! 'set!)
(define kwd:define 'define)
(define kwd:lambda 'lambda)
(define kwd:quote 'quote)

; eof
