; -*- Scheme -*-
;
; Scheme 313 run-time system
; The `error' procedure, more or less as defined in Chez Scheme.
;
; `error' takes two fixed arguments and then a variable number of extra 
; arguments. The first argument is a symbol; it is typically the name of 
; the procedure in which the error occurred. The second argument is a format
; string. The remaining arguments are arguments to the formats in the format
; string. The string is formatted with the arguments; excess arguments are
; attached to the end of the resulting string (with spaces inbetween).
;
; If the first argument is null, then it is not printed at all.
;
; The only format specifier allowed at this time is ~a.

(define *scheme313-error-handler* exit)

(define (error proc msg . args)

  (define (simple-format msg arglist)
    (let loop ((i 0) (j (string-length msg)) (l arglist))
      (if (< i j)
	  (if (not (char=? (string-ref msg i) #\~))
	      (begin (display (string-ref msg i))
		     (loop (+ i 1) j l))
	      (begin (if (< i (- j 1))
			 (let ((f (string-ref msg (+ i 1))))
			   (if (char=? f #\a)
			       (begin (display (car l))
				      (loop (+ i 2) j (cdr l)))
			       (begin (display #\~)
				      (loop (+ i 1) j l))))
			 (begin (display #\~)
				(loop (+ i 1) j l)))))
	  (for-each (lambda (x) (display " ") (display x)) l))))

  (display "Error")
  (if (not (null? proc))
      (begin (display " in procedure `")
	     (display proc)
	     (display "': "))
      (display ": "))
  (simple-format msg args)
  (newline)
  (*scheme313-error-handler*))

