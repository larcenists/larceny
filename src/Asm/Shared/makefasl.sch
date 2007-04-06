; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Procedure that writes fastload segment.
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

(define (dump-fasl-segment-to-port segment outp . rest)
  (let* ((omit-code? (not (null? rest)))
         (controllify
	  (lambda (char)
	    (integer->char (- (char->integer char) (char->integer #\@)))))
	 (CTRLP       (controllify #\P))
	 (CTRLB       (controllify #\B))
	 (CTRLG       (controllify #\G))
	 (DOUBLEQUOTE (char->integer #\"))
	 (BACKSLASH   (char->integer #\\))
	 (len         1024))

    (define buffer (make-bytevector len))
    (define ptr 0)

    (define (flush)
      (if (< ptr len)
	  (write-bytevector-like (sub-bytevector buffer 0 ptr) outp)
	  (write-bytevector-like buffer outp))
      (set! ptr 0))

    ; Returns a newly allocated bytevector
    ; containing bytes [i,j) of the given bytevector.

    (define (sub-bytevector bv i j)
      (let ((bv2 (make-bytevector (- j i))))
        (do ((i i (+ i 1))
             (k 0 (+ k 1)))
            ((= i j) bv2)
          (bytevector-set! bv2 k (bytevector-ref bv i)))))

    ; Outputs the encoding of an Ascii character.

    (define (putc c)
      (if (= ptr len) (flush))
      (bytevector-set! buffer ptr (char->integer c))
      (set! ptr (+ ptr 1)))

    (define (putb b)
      (if (= ptr len) (flush))
      (bytevector-set! buffer ptr b)
      (set! ptr (+ ptr 1)))

    ; Outputs the encoding of an Ascii string.

    (define (puts s)
      (let ((ls (string-length s)))
	(if (>= (+ ptr ls) len)
	    (begin (flush)
		   (write-string s outp))
	    (do ((i (- ls 1) (- i 1))
		 (p (+ ptr ls -1) (- p 1)))
		((< i 0)
		 (set! ptr (+ ptr ls)))
	      (bytevector-set! buffer p (char->integer (string-ref s i)))))))

    (define (putd d)
      (flush)
      (write-fasl-datum d outp))

    (define (dump-codevec bv)
      (if omit-code?
          (puts "#f")
          (begin
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
                  (putb c)))))))

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
      (if (not omit-code?) (putc #\())
      (putc #\#)
      (putc CTRLP)
      (putc #\()
      (dump-codevec (car segment))
      (putc #\space)
      (dump-constvec (cdr segment))
      (puts " #f)")
      (if (not omit-code?) (putc #\)))
      (putc #\newline))

    (if (pair? segment)			; Ignore "declaration" strings
	(begin (dump-fasl-segment segment)
	       (flush)))))

; eof
