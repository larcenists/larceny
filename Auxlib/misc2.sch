; More useful procedures.
; $Id: misc2.sch,v 1.1 1997/09/17 15:06:05 lth Exp lth $

; Most-positive-fixnum and most-negative-fixnum are of course implementation-
; dependent, but for now there's only one implementation :-)

(define most-positive-fixnum
  (let ((mpf (- (expt 2 29) 1)))
    (lambda ()
      mpf)))

(define most-negative-fixnum
  (let ((mnf (expt -2 29)))
    (lambda ()
      mnf)))

; Read-substring and write-substring can be optimized by plugging into 
; the I/O system; for now, they are useful abstractions.

; Returns #<eof> if no characters were read, otherwise the number of
; characters that were read into the string.

(define (read-substring buf start end . rest)
  (let ((port (cond ((null? rest) (current-input-port))
		    ((null? (cdr rest)) (car rest))
		    (else (error "read-substring: too many arguments.")))))
    (let loop ((i start))
      (if (= i end)
	  (- end start)
	  (let ((c (read-char port)))
	    (if (eof-object? c)
		(if (= i start)
		    c
		    (- i start))
		(begin (string-set! buf i c)
		       (loop (+ i 1)))))))))

; Returns something unspecified.

(define (write-substring buf start end . rest)
  (let ((port (cond ((null? rest) (current-input-port))
		    ((null? (cdr rest)) (car rest))
		    (else (error "read-substring: too many arguments.")))))
    (do ((i start (+ i 1)))
	((= i end))
      (write-char (string-ref buf i) port))))

; eof
