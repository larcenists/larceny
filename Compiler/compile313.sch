; Compiler/compile313.sch
; Larceny run-time environment -- compiler drivers.
;
; $Id: compile313.sch,v 1.4 1997/08/22 21:00:04 lth Exp $
;
; We operate with several different file formats.
;
; - Files with extension ".sch" or ".scm" are Scheme source files and are
;   processed with the command "compile313".
;
; - Files with extension ".lap" (Lisp Assembly Program) are tokenized
;   MacScheme assembly language files; all opcodes are integerized. They are
;   processed with the command "assemble313".
;
; - Files with extension ".mal" (MacScheme Assembly Language") are untokenized
;   MacScheme assembly language files. Each expression in the file turns into
;   a ".lap" expression when evaluated by "eval". (Yeah, this is a hack.)
;   These files are processed with the command "assemble313".
;
; - Files with extension ".lop" (Lisp Object Program) or ".elop" are 
;   tokenized files in the target architecture's instruction set (elop files
;   are compiled without write barrier checks). In this representation,
;   all procedures are represented by a segment, the car of which is the
;   code vector (a sequence of raw opcode bytes) and the cdr of which is the
;   constant vector (in symbolic form, still). Segments are transformed into
;   heaps by the heap loader.
;   These files can be disassembled (to native assembly language) with the
;   command "disassemble313".
;
; - Files with extension ".fasl" (Fastload) are binary, dynamically-loadable
;   compiled files.

;; Compile and assemble a '.sch' or '.scm' file and produce a '.fasl' file.

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
		    (assemble (compile item))))
    #t))


;; Compile a scheme file (extension ".sch" or ".scm") to a ".lap" file.

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
		    ; (break)
		    (compile x)))
    #t))


;; Assemble a ".lap" or ".mal" file to a ".lop" file.

(define (assemble313 file . rest)
  (let ((outputfile
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type file
				".lap"
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
		      (lambda (x)
			; (break)
			(assemble x))))
    #t))


;; Converts each segment into a call to a literal thunk, where the thunk is 
;; a literal procedure (using magic syntax; see file "makefasl.sch").

(define (make-fasl infilename . rest)
  (let ((outfilename
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type
	      infilename
	      ".lop"
	      (if (write-barrier)
		  ".fasl"
		  ".efasl")))))
    (process-file infilename
		  outfilename
		  dump-fasl-segment-to-port
		  (lambda (x) x))
    #t))


; Disassemble a file; dump output to screen or other (optional) file.

(define (disassemble313 file . rest)

  (define (print . rest)
    (for-each display rest)
    (newline))

  (define (print-segment segment-no segment)

    (define (print-constvector cv)
      (let loop ((i 0))
	(if (< i (vector-length cv))
	    (begin
	      (print "------------------------------------------")
	      (print "Constant vector element # " i)
	      (case (car (vector-ref cv i))
		((codevector)
		 (print "Code vector")
		 (print-ilist (disassemble (cadr (vector-ref cv i)))))
		((constantvector)	
		 (print "Constant vector")
		 (print-constvector (cadr (vector-ref cv i))))
		((global)
		 (print "Global: " (cadr (vector-ref cv i))))
		((data)
		 (print "Data: " (cadr (vector-ref cv i)))))
	      (loop (+ i 1))))))

    (print "Segment # " segment-no)
    (print-ilist (disassemble (car segment)))
    (print-constvector (cdr segment))
    (print "========================================"))

  (define (doit file)
    (print "; From " file)
    (call-with-input-file
      file
      (lambda (inport)
	(let loop ((segment-no 1) (segment (read inport)))
	  (if (eof-object? segment)
	      '()
	      (begin (print-segment segment-no segment)
		     (loop (+ segment-no 1) (read inport))))))))

  ; disassemble313

  (if (or (and (not (null? rest))
	       (or (not (null? (cdr rest)))
		   (not (string? (car rest)))))
	  (not (string? file)))
      (error 'disassemble313 "Bogus parameter(s)."))
  (if (null? rest)
      (doit file)
      (begin
	(delete-file (car rest))
	(with-output-to-file (car rest) (lambda () (doit file))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some utilities



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
			    (string=? (substring filename (- j l) j) n))
		       (string-append (substring filename 0 (- j l)) new)
		       (loop (cdr m))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Display some integer in an mostly arbitrary base to some arbitrary 
; precision. I forget how one is supposed to use this.

(define (display-in-base n m precision)
  (let ((v '#(0 1 2 3 4 5 6 7 8 9 #\A #\B #\C #\D #\E #\F)))
    (if (zero? n)
	(if (zero? precision)
	    '()
	    (begin (display 0)
		   (display-in-base n m (- precision 1))))
	(begin (display-in-base (quotient n m) m (- precision 1))
	       (display (vector-ref v (remainder n m)))))))

; Macros used in the MacScheme version to reduce the size of the heap image.

; (define-macro optimize
;   (lambda (l)
;     (list 'declare
;           (list 'optimize
;                 (cond ((and (memq 'safety l) (memq 'speed l)) 2)
;                       ((memq 'speed l) 3)
;                       ((memq 'space l) 1)
;                       ((memq 'safety l) 1)
;                       (else (optimization)))))))
; 
; (define-macro optimize
;   (lambda (l)
;     '(declare (optimize 1)))) ; for now
; 
; 
; 
