; Bits and pieces of the Larceny debugger
;
; $Id: debug.sch,v 1.2 1992/05/15 22:18:00 lth Exp $

; Called by the exception handler for undefined globals.
;
; The argument is a pointer to a global cell, the cdr of which has the name
; of the cell.

(define (undefined-global-exception cell)
  (let ((n (cdr cell)))
    (display "Undefined global variable ")
    (if (symbol? n)
	(display n)
	(begin (display "#")
	       (display n)))
    (newline)
    (debugger-loop)))

(define (debugger-loop)

  (define (loop)
    (display "*> ")
    (flush-output-port)
    (let ((cmd (read)))
      (if (not (eof-object? cmd))
	  (case cmd
	    ((exit) (exit))
	    ((return) #t)
	    ((help ?)
	     (display "exit      -- leave Larceny") (newline)
	     (display "return    -- return to the running program") (newline)
	     (loop))
	    (else 
	     (display "Nah.")
	     (newline) 
	     (loop))))))

  (display "Enter ? for help.") (newline)
  (loop))
