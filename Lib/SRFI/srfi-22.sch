; SRFI 22: Running scheme scripts on Unix
; 2004-01-03 / lth
;
; $Id$
;
; To use
;
; This file must be loaded from .larceny or dumped as part of the heap
; image, otherwise the code that processes the #! prefix will not be
; installed early enough and the script will not be loaded properly.
;
; In my .larceny I have this line:
;    (load "/home/lth/lib/srfi/srfi-22.sch")
;
; If srfi-0 or srfi-7 are requested, then the REQUIRE package must be
; loaded by .larceny as well, because the code below uses REQUIRE to
; load those packages.  Put something like this in .larceny:
;    (load "/home/lth/lib/require0.sch")
;
; Interactive use of SRFI-22 (ie loading a script containing the
; script prelude and expecting MAIN to be called upon loading) is not
; currently supported, see Bugs section below.
;
;
; Trampoline programs
;
; The companion trampoline program scheme-r5rs.c can be compiled with
; the local C compiler and installed somewhere in your path.  Hard
; links to that program under other names (scheme-r4rs, scheme-srfi-0,
; etc.) are sufficient to enable the use of the various dialects.
;
;
; Bugs
;
; Buglet: does not play nice with other reader macros for "#!".  Can
; this be fixed easily?  We need the ability to look ahead more than
; one character.
;
; Buglet: if the script is loaded into an interactive system the MAIN
; procedure will be called but then EXIT will be called.  It would be
; better if it would just return the exit status to the caller.
;
; Buglet: if the script is loaded into an interactive system the
; nulling out of the srfi-22 procedure in the code will prevent
; loading multiple scripts.

(require 'fluid)

(let* ((sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader))
       (space-reader (readtable-ref #\space))
       (space-dispatch (cadr space-reader))
       (space-dispatch-list (caddr space-reader)))

  (define (fixfn script-name)
    (let ((path (let loop ((i (- (string-length script-name) 1)))
		  (cond ((< i 0) "")
			((char=? (string-ref script-name i) #\/)
			 (substring script-name 0 (+ i 1)))
			(else
			 (loop (- i 1)))))))
      (lambda (fn)
	(if (char=? #\/ (string-ref fn 0))
	    fn
	    (string-append path fn)))))

  (define (srfi-22)
    (let ((exited #f)
	  (EX_SOFTWARE 70))
      (dynamic-wind
	  (lambda () #t)
	  (lambda () 
	    (let ((args (vector->list (command-line-arguments))))
	      (set! srfi-22 (lambda args #t))
	      (cond ((string=? "srfi-0" (car args)) 
		     (require 'srfi-0)
		     (load (cadr args)))
		    ((string=? "srfi-7" (car args))
		     (require 'srfi-0)
		     (require 'srfi-7)
		     (fluid-let ((*srfi-7-filename-processor* (fixfn
							       (cadr args))))
		       (load (cadr args))))
		    (else
		     (load (cadr args))))
	      (set! exited (main (cdr args)))))
	  (lambda ()
	    (if (or (not exited)
		    (not (number? exited))
		    (not (exact? exited))
		    (not (integer? exited)))
		(exit EX_SOFTWARE)
		(exit exited))))))

  (define (read-comment p)
    (let ((c (peek-char p)))
      (if (eqv? c #\!)
          (begin
            (read-char p)
            (let ((c (peek-char p)))
              (if (and (= (port-position p) 2)
		       (eq? c #\space))
		  (let loop ((c (read-char p)))
		    (if (or (eof-object? c) (char=? c #\newline))
			(srfi-22)
			(loop (read-char p))))
                  (let ((s (read p)))
                    (case s
                      ((unspecified) '#!unspecified)
                      ((undefined)   '#!undefined)
                      ((null)        '())
                      ((true)        #t)
                      ((false)       #f)
                      (else
                       (error "Unknown #! constant: " s)))))))
          #f)))

  (define (new-sharp-dispatch c p)
    (if (read-comment p)
        (space-dispatch #\space p)
        (sharp-dispatch c p)))

  (define (new-sharp-dispatch-list c p)
    (if (read-comment p)
        (space-dispatch-list #\space p)
        (sharp-dispatch-list c p)))

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            new-sharp-dispatch-list))
  'srfi-22)



	  