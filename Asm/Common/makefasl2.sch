; Asm/Common/makefasl2.sch
; Larceny -- procedure that writes fastload segment
;
; $Id: makefasl2.sch,v 1.1.1.1 1998/11/19 21:51:58 lth Exp $
;
; The procedure 'dump-fasl-segment-to-port' takes a segment and an output
; port as arguments and dumps the segment in fastload format on that port.
; The port must be a binary (untranslated) port.
;
; A fastload segment looks like a Scheme expression, and in fact, 
; fastload files can mix compiled and uncompiled expressions.  A compiled
; expression (as created by dump-fasl-segment-to-port) is a list with
; a literal procedure in the operator position and no arguments.
;
; A literal procedure is a three-element list prefixed by #^P.  The three
; elements are code (a bytevector), constants (a regular vector), and
; R0/static link slot (always #f).  
;
; A bytevector is a string prefixed by #^B. The string may contain 
; control characters; \ and " must be quoted as usual.
;
; A global variable reference in the constant vector is a symbol prefixed
; by #^G.  On reading, the reference is replaced by (a pointer to) the 
; actual cell.
;
; This code is highly bummed.  The procedure write-bytevector-like has the
; same meaning as display, but in Larceny, the former is currently much
; faster than the latter.

(define (dump-fasl-segment-to-port segment outp)
  (let* ((controllify
	  (lambda (char)
	    (integer->char (- (char->integer char) (char->integer #\@)))))
	 (CTRLP       (controllify #\P))
	 (CTRLB       (controllify #\B))
	 (CTRLG       (controllify #\G))
	 (DOUBLEQUOTE (char->integer #\"))
	 (BACKSLASH   (char->integer #\\))
	 (len         1024))

    (define buffer (make-string len #\&))
    (define ptr 0)

    (define (flush)
      (if (< ptr len)
	  (write-bytevector-like (substring buffer 0 ptr) outp)
	  (write-bytevector-like buffer outp))
      (set! ptr 0))

    (define (putc c)
      (if (= ptr len) (flush))
      (string-set! buffer ptr c)
      (set! ptr (+ ptr 1)))

    (define (putb b)
      (if (= ptr len) (flush))
      (string-set! buffer ptr (integer->char b))
      (set! ptr (+ ptr 1)))

    (define (puts s)
      (let ((ls (string-length s)))
	(if (>= (+ ptr ls) len)
	    (begin (flush)
		   (write-bytevector-like s outp))
	    (do ((i (- ls 1) (- i 1))
		 (p (+ ptr ls -1) (- p 1)))
		((< i 0)
		 (set! ptr (+ ptr ls)))
	      (string-set! buffer p (string-ref s i))))))

    (define (putd d)
      (flush)
      (write-fasl-datum d outp))

    (define (dump-codevec bv)
      (putc #\#)
      (putc CTRLB)
      (putc #\")
      (let ((limit (bytevector-length bv)))
	(do ((i 0 (+ i 1)))
	    ((= i limit) (putc #\")
			 (putc #\newline))
	  (let ((c (bytevector-ref bv i)))
	    (cond ((= c DOUBLEQUOTE) (putc #\\))
		  ((= c BACKSLASH)   (putc #\\)))
	    (putb c)))))

    (define (dump-constvec cv)
      (puts "#(")
      (for-each (lambda (const)
		  (putc #\space)
		  (case (car const)
		    ((data)
		     (putd (cadr const)))
		    ((constantvector)
		     (dump-constvec (cadr const)))
		    ((codevector)
		     (dump-codevec (cadr const)))
		    ((global)
		     (putc #\#)
		     (putc CTRLG)
		     (putd (cadr const)))
		    ((bits)
		     (error "BITS attribute is not supported in fasl files."))
		    (else
		     (error "Faulty .lop file."))))
		(vector->list cv))
      (puts ")")
      (putc #\newline))

    (define (dump-fasl-segment segment)
      (puts "(#")
      (putc CTRLP)
      (putc #\()
      (dump-codevec (car segment))
      (putc #\space)
      (dump-constvec (cdr segment))
      (puts " #f))")
      (putc #\newline))

    (dump-fasl-segment segment)
    (flush)))

; eof
