; Batch compiler/assembler/disassembler drivers for Scheme 313.
;
; $Id: compile313.sch,v 1.3 91/10/13 21:16:50 lth Exp Locker: lth $
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
; - Files with extension ".lop" (Lisp Object Program) are tokenized files
;   in the target architecture's instruction set. In this representation,
;   all procedures are represented by a segment, the car of which is the
;   code vector (a sequence of raw opcode bytes) and the cdr of which is the
;   constant vector (in symbolic form, still). Segments are transformed into
;   heaps by the heap loader.
;   These files can be disassembled (to native assembly language) with the
;   command "disassemble313".

; Compile a scheme file (extension ".sch" or ".scm") to a ".lap" file.

(define (compile313 file)
  (let* ((n (string-length file))
         (outputfile
          (string-append
           (if (and (> n 4)
                    (or (string-ci=? ".sch" (substring file (- n 4) n))
			(string-ci=? ".scm" (substring file (- n 4) n))))
               (substring file 0 (- n 4))
               file)
           ".lap")))
    (call-with-input-file
     file
     (lambda (p)
       (delete-file outputfile)
       (call-with-output-file
        outputfile
        (lambda (q)
          (display "\; Compiled from " q)
          (display file q)
          (newline q)
          (newline q)
          (do ((x (read p) (read p)))
              ((eof-object? x))
              (write-codelist (compile x) q)
              (newline q)
              (newline q))))))))


; Assemble a ".lap" or ".mal" file to a ".lop" file.

(define (assemble313 file)

  (define (assemble-file filter infile outfile)

    (define (file-for-each proc filename)
      (call-with-input-file filename
	(lambda (p)
	  (let loop ((x (read p)))
	    (if (eof-object? x)
		#t
		(begin (proc x)
		       (loop (read p))))))))

    (delete-file outfile)
    (let ((p (open-output-file outfile)))
      (file-for-each (lambda (x) (write (assemble (filter x)) p)) infile)
      (close-output-port p)
      #t))

  ; assemble313

  (let* ((n (string-length file))
         (outputfile
          (string-append
           (if (and (> n 4)
                    (or (string-ci=? ".lap" (substring file (- n 4) n))
			(string-ci=? ".mal" (substring file (- n 4) n))))
               (substring file 0 (- n 4))
               file)
           ".lop")))
    (if (and (> n 4) (string-ci=? ".mal" (substring file (- n 4) n)))
	(assemble-file eval file outputfile)
	(assemble-file (lambda (x) x) file outputfile))))


; Disassemble a file; dump output to screen or other (optional) file.

(define (disassemble313 file . rest)

  (define (print-segment segment-no segment)

    (define (print-constvector cv)
      (let loop ((i 0))
	(if (< i (vector-length cv))
	    (begin
	      (display (format "------------------------------------------~%"))
	      (display (format "Constant vector element # ~a~%" i))
	      (case (car (vector-ref cv i))
		((codevector)
		 (display (format "Code vector~%"))
		 (print-ilist (disassemble (cadr (vector-ref cv i)))))
		((constantvector)	
		 (display (format "Constant vector~%"))
		 (print-constvector (cadr (vector-ref cv i))))
		((global)
		 (display (format "Global: ~a~%" (cadr (vector-ref cv i)))))
		((data)
		 (display (format "Data: ~a~%" (cadr (vector-ref cv i))))))
	      (loop (+ i 1))))))

    (display (format "Segment # ~a~%" segment-no))
    (print-ilist (disassemble (car segment)))
    (print-constvector (cdr segment))
    (display (format "========================================~%")))

  (define (doit file)
    (display (format "; From ~a~%" file))
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
