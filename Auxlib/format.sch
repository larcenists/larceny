; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Useful formatting procedures.

(define (format-left-justified x n)
  (let* ((x (printable-representation x))
	 (l (string-length x)))
    (if (>= l n)
	x
	(string-append x (make-string (- n l) #\space)))))

(define (format-right-justified x n)
  (let* ((x (printable-representation x))
	 (l (string-length x)))
    (if (>= l n)
	x
	(string-append (make-string (- n l) #\space) x))))

(define (printable-representation x)
  (let ((p (open-output-string)))
    (display x p)
    (get-output-string p)))

; eof
