;; Larceny run-time environment -- procedure to create loadable file.
;;
;; This file exports the procedure 'dump-fasl-segment-to-port'. It takes
;; a segment and an output port as arguments. It is way slow.
;;
;; History
;;   July 19, 1994 / lth (v0.20)
;;     The procedure 'make-fasl' is no longer in this file, but in the
;;     driver file.
;;
;;   Long time ago / lth
;;     Created.
;;
;; The format of the fastload file is one of a sequence of expressions.
;; Procedures, bytevectors, and references to global variables can be
;; embedded directly in the file via magic syntax.
;;
;; Magic syntax:
;;  - A procedure is a list prefixed by #^P.
;;  - A code vector is a string prefixed by #^B. The string may contain 
;;    control characters; \ and " must be quoted as usual. There are some
;;    subtle dependencies here on Unix I/O semantics: no control characters
;;    are translated on output or input.
;;  - A global variable cell is a symbol prefixed by #^G. On reading, the
;;    reference is replaced by (a pointer to) the actual cell, with the 
;;    context being the current top-level environment.

(define (dump-fasl-segment-to-port segment outp)
  (let* ((controllify
	  (lambda (char)
	    (integer->char (- (char->integer char) (char->integer #\@)))))
	 (CTRLP (controllify #\P))
	 (CTRLB (controllify #\B))
	 (CTRLG (controllify #\G)))

    ;; Everyone calls putc/puts/putd; these can then be optimized.

    (define (putc c)
      (write-char c outp))

    (define (puts s)
      (for-each (lambda (c)
		  (write-char c outp))
		(string->list s)))

    (define (putd d)
      (write d outp))

    ;; Should use bytevectors; optimized for Chez.

    (define (dump-codevec bv)
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
		     (error "The BITS attribute is not supported in fasl files."))
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
