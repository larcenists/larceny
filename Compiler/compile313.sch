; Compiler/compile313.sch
; Larceny run-time environment -- compiler drivers
;
; $Id: compile313.sch,v 1.5 1997/09/23 20:06:36 lth Exp lth $
;
; The meanings of the various file types are explained in the on-line
; documentation (http://www.ccs.neu.edu/home/larceny/manual.html).

; Auxiliary installable macro-expander (for Doug, mainly).
; Can go away when we have the new compiler.

(define *custom-expander* (lambda (x) x))

(define (twobit-auxiliary-expander . rest)
  (cond ((null? rest)
	 *custom-expander*)
	((null? (cdr rest))
	 (set! *custom-expander* (car rest))
	 *custom-expander*)
	(else
	 (error "twobit-auxiliary-expander: too many arguments."))))


; Compile and assemble a scheme source file and produce a fastload file.

(define (compile-file infilename . rest)
  (let ((outfilename
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type infilename
				'(".sch" ".scm")
				(if (write-barrier)
				    ".fasl"
				    ".efasl")))))
    (process-file infilename
		  outfilename
		  dump-fasl-segment-to-port
		  (lambda (item)
		    (assemble (compile ((twobit-auxiliary-expander) item)))))
    (unspecified)))


; Assemble a '.mal' or '.lop' file and produce a fastload file.

(define (assemble-file infilename . rest)
  (let ((outfilename
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type infilename
				'(".lap" ".mal")
				(if (write-barrier)
				    ".fasl"
				    ".efasl"))))
	(malfile?
	 (and (> n 4)
	      (string-ci=? ".mal" (substring file (- n 4) n)))))
    (process-file infilename
		  outfilename
		  dump-fasl-segment-to-port
		  (lambda (item)
		    (if malfile? 
			(assemble (eval item))
			(assemble item))))
    (unspecified)))


; Compile a scheme source file to a ".lap" file.

(define (compile313 file . rest)
  (let ((outputfile
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type file '(".sch" ".scm") ".lap"))))
    (process-file file
		  outputfile
		  (lambda (item port)
		    (write item port)
		    (newline port)
		    (newline port))
		  (lambda (x)
		    (compile ((twobit-auxiliary-expander) x))))
    (unspecified)))


; Assemble a ".lap" or ".mal" file to a ".lop" file.

(define (assemble313 file . rest)
  (let ((outputfile
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type file
				'(".lap" ".mal")
				(if (write-barrier)
				    ".lop"
				    ".elop"))))
	(n (string-length file)))
    (process-file file
		  outputfile
		  write-lop
		  (if (and (> n 4)
			   (string-ci=? ".mal" (substring file (- n 4) n)))
		      (lambda (x) (assemble (eval x)))
		      assemble))
    (unspecified)))


; Convert a ".lop" file to fastload format.

(define (make-fasl infilename . rest)
  (let ((outfilename
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type infilename
				'(".lop" ".elop")
				(if (write-barrier)
				    ".fasl"
				    ".efasl")))))
    (process-file infilename
		  outfilename
		  dump-fasl-segment-to-port
		  (lambda (x) x))
    (unspecified)))


; Disassemble a procedure's code vector.

(define (disassemble item . rest)
  (let ((output-port (if (null? rest)
			 (current-output-port)
			 (car rest))))
    (disassemble-item item #f output-port)
    (unspecified)))


; The item can be either a procedure or a pair (assumed to be a segment).

(define (disassemble-item item segment-no port)

  (define (print . rest)
    (for-each (lambda (x) (display x port)) rest)
    (newline port))

  (define (print-constvector cv)
    (do ((i 0 (+ i 1)))
	((= i (vector-length cv)))
      (print "------------------------------------------")
      (print "Constant vector element # " i)
      (case (car (vector-ref cv i))
	((codevector)
	 (print "Code vector")
	 (print-instructions (disassemble-codevector
			      (cadr (vector-ref cv i)))))
	((constantvector)	
	 (print "Constant vector")
	 (print-constvector (cadr (vector-ref cv i))))
	((global)
	 (print "Global: " (cadr (vector-ref cv i))))
	((data)
	 (print "Data: " (cadr (vector-ref cv i)))))))

  (define (print-segment segment)
    (print "Segment # " segment-no)
    (print-instructions (disassemble-codevector (car segment)))
    (print-constvector (cdr segment))
    (print "========================================"))

  (cond ((procedure? item)
	 (print-instructions (disassemble-codevector (procedure-ref item 0))))
	((and (pair? item)
	      (bytevector? (car item))
	      (vector? (cdr item)))
	 (print-segment item))
	(else
	 (error "disassemble-item: " item " is not disassemblable."))))


; Disassemble a ".lop" or ".fasl" file; dump output to screen or 
; other (optional) file.

(define (disassemble-file file . rest)

  (define (print . rest)
    (for-each (lambda (x) (display x port)) rest)
    (newline port))

  (define (doit input-port output-port)
    (display "; From " output-port)
    (display file output-port)
    (newline output-port)
    (do ((segment-no 0 (+ segment-no 1))
	 (segment (read input-port) (read input-port)))
	((eof-object? segment))
      (disassemble-item segment segment-no output-port)))

  ; disassemble313

  (call-with-input-file file
    (lambda (input-port)
      (if (null? rest)
	  (doit input-port (current-output-port))
	  (begin
	    (delete-file (car rest))
	    (call-with-output-file (car rest)
	      (lambda (output-port) (doit file output-port)))))))
  (unspecified))


; Meta-switches

(define target-architecture
  (let ((arch 'sun4-sunos)
	(arch-list '(sun4-sunos sun4-solaris unknown)))
    (lambda rest
      (cond ((null? rest)
	     arch)
	    ((and (null? (cdr rest))
		  (memq (car rest) arch-list))
	     (set! arch (car rest))
	     arch)
	    ((and (null? (cdr rest))
		  (eq? (car rest) 'query))
	     arch-list)
	    (else
	     (error "Wrong arguments to target-architecture: " rest))))))

(define (fast-unsafe-code)
  (integrate-usual-procedures #t)
  (benchmark-mode #t)
  (inline-cons #t)
  (inline-assignment #t)
  (catch-undefined-globals #f)
  (unsafe-code #t)
  (single-stepping #f))

(define (fast-safe-code)
  (integrate-usual-procedures #t)
  (benchmark-mode #t)
  (inline-cons #t)
  (inline-assignment #t)
  (catch-undefined-globals #t)
  (unsafe-code #f)
  (single-stepping #f))


; Read and process one file, producing another.

(define (process-file infilename outfilename writer processer)
  (delete-file outfilename)
  (call-with-output-file outfilename
    (lambda (outport)
      (call-with-input-file infilename
	(lambda (inport)
	  (let loop ((x (read inport)))
	    (if (eof-object? x)
		#t
		(begin (writer (processer x) outport)
		       (loop (read inport))))))))))


; Given a file name with some type, produce another with some other type.

(define (rewrite-file-type filename matches new)
  (if (not (pair? matches))
      (rewrite-file-type filename (list matches) new)
      (let ((j (string-length filename)))
	(let loop ((m matches))
	  (cond ((null? m)
		 (string-append filename new))
		(else
		 (let* ((n (car m))
			(l (string-length n)))
		   (if (and (>= j l)
			    (string-ci=? (substring filename (- j l) j) n))
		       (string-append (substring filename 0 (- j l)) new)
		       (loop (cdr m))))))))))

; eof
