; Asm/Common/makefasl.sch
; Larceny -- procedure to create fastload segments.
;
; $Id$
;
; The procedure 'dump-fasl-segment-to-port' takes a segment and an output
; port as arguments and dumps the segment in fastload format on that port.
;
; The global variable host-system must be set to the name of the host
; system for the compilation, 'chez or 'larceny.
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
; control characters; \ and " must be quoted as usual. There are some
; subtle dependencies here on Unix I/O semantics: no control characters
; are translated on output or input.
;
; A global variable reference in the constant vector is a symbol prefixed
; by #^G.  On reading, the reference is replaced by (a pointer to) the 
; actual cell.
;
; BUGS
;  - Probably quite slow, but serious improvement requires cooperation
;    of the I/O system for block transfers.

(define (dump-fasl-segment-to-port segment outp)
  (let* ((controllify
	  (lambda (char)
	    (integer->char (- (char->integer char) (char->integer #\@)))))
	 (CTRLP       (controllify #\P))
	 (CTRLB       (controllify #\B))
	 (CTRLG       (controllify #\G))
	 (DOUBLEQUOTE (char->integer #\"))
	 (BACKSLASH   (char->integer #\\)))

    ; Everyone calls putc/puts/putd/putb; these can then be optimized.

    (define (putc c)
      (write-char c outp))

    (define (putb b)
      (write-char (integer->char b) outp))

    (define (puts s)
      (for-each (lambda (c)
		  (write-char c outp))
		(string->list s)))

    (define (putd d)
      (write d outp))

    (define (dump-codevec bv)
      (cond ((eq? host-system 'chez)
	     (dump-codevec-chez bv))
	    ((eq? host-system 'larceny)
	     (dump-codevec-larceny bv))
	    (else (error "makefasl: unknown host: " host-system))))

    ; Under Chez Scheme, byte vectors are really vectors of integers.

    (define (dump-codevec-chez bv)
      (putc #\#)
      (putc CTRLB)
      (putc #\")
      (let loop ((i 0) (m (vector-length bv)))
	(if (< i m)
	    (let ((c (integer->char (vector-ref bv i))))
	      (if (or (char=? c #\") (char=? c #\\))
		  (putc #\\))
	      (putc c)
	      (loop (+ i 1) m))))
      (putc #\")
      (putc #\newline))

    (define (dump-codevec-larceny bv)
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

    (dump-fasl-segment segment)))

; eof
