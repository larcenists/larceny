; Bits and pieces of the Larceny debugger
;
; $Id: debug.sch,v 1.1 1995/08/01 04:45:56 lth Exp lth $

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

; simple, slow trace facility

(define sys$enabled #f)
(define sys$traces '())

(define (sys$tracectl type)
  (cond ((eq? type 'get)
	 sys$traces)
	((eq? type 'start)
	 (set! sys$enabled #t))
	((eq? type 'stop)
	 (set! sys$enabled #f))
	((eq? type 'clear)
	 (set! sys$traces '()))
	(else
	 (error "sys$tracectl: unknown: " type))))

(define (sys$trace item)
  (if sys$enabled
      (let* ((probe (assq item sys$traces))
	     (i     (if probe
			probe
			(let ((p (cons item 0)))
			  (set! sys$traces (cons p sys$traces))
			  p))))
	(set-cdr! i (+ (cdr i) 1)))))

; eof
