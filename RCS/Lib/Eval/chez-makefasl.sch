;; Given the name of a .lop file, creates a .fasl file.
;; This program to run under systems which do not have bytevectors, like Chez.
;;
;; $Id$
;;
;; The format of the fastload file is one of a sequence of expressions.
;; Procedures, bytevectors, and references to global variables can be
;; embedded directly in the file via magic syntax.
;;
;; Magic syntax:
;;  - A procedure is a list prefixed by #^P.
;;  - A code vector is a string prefixed by #^B. The string may contain 
;;    control characters; \ and " must be quoted as usual.
;;  - A global variable cell is a symbol prefixed by #^G. On reading, the
;;    reference is replaced by (a pointer to) the actual cell, with the 
;;    context being the current top-level environment.
;;
;; An alternative way of accomplishing the resolution of global variables would
;; be to use (procedure <cvec> (vector <const> <global> ...) #f) rather than
;; the magic syntax; after all, the reader performs these operations anyway.
;; The ^B magic syntax should be used anyway, as there are no substructures
;; which need to be resolved. The reader would not need to know about the
;; top-level environment.

(define (controllify char)
  (integer->char (- (char->integer char) (char->integer #\@))))

(define CTRLP (controllify #\P))
(define CTRLB (controllify #\B))
(define CTRLG (controllify #\G))

; Converts each segment into a call to a literal thunk, where the thunk is 
; a literal procedure (represented via magic syntax).

(define (make-fasl infilename)
  (let ((outfilename
	 (if (and (>= (string-length infilename) 4)
		  (string=? (substring infilename 
				       (- (string-length infilename) 4)
				       (string-length infilename))
			    ".lop"))
	     (string-append (substring infilename 
				       0 
				       (- (string-length infilename) 4))
			    ".fasl")
	     (string-append infilename ".fasl"))))
    (delete-file outfilename)
    (let ((inp  (open-input-file infilename))
	  (outp (open-output-file outfilename)))
      (let loop ((segment (read inp)))
	(if (not (eof-object? segment))
	    (begin
	      (display "(" outp)
	      (display "#" outp)
	      (display CTRLP outp)
	      (display "(" outp)
	      (dump-codevec (car segment) outp)
	      (display " " outp)
	      (dump-constvec (cdr segment) outp)
	      (display " " outp)
	      (display "#f)" outp)
	      (display ")" outp)
	      (newline outp)
	      (loop (read inp)))
	    (begin
	      (close-input-port inp)
	      (close-output-port outp)
	      #t))))))


(define (dump-codevec bv outp)
  (display "#" outp)
  (display CTRLB outp)
  (display #\" outp)
  (let loop ((i 0))
    (if (< i (bytevector-length bv))
	(let ((c (integer->char (bytevector-ref bv i))))
	  (if (or (char=? c #\") (char=? c #\\))
	      (display #\\ outp))
	  (display c outp)
	  (loop (+ i 1)))
	(display #\" outp))))


(define (dump-constvec cv outp)
  (display "#(" outp)
  (for-each (lambda (const)
	      (display " " outp)
	      (case (car const)
		((data)
		 (write (cadr const) outp))
		((constantvector)
		 (dump-constvec (cadr const) outp))
		((codevector)
		 (dump-codevec (cadr const) outp))
		((global)
		 (display "#" outp)
		 (display CTRLG outp)
		 (write (cadr const) outp))
		((bits)
		 (error "The BITS attribute is not supported in fasl files."))
		(else
		 (error "Faulty .lop file."))))
	    (vector->list cv))
  (display ")" outp))

; eof
