; Compiler/assembler drivers for Scheme 313 under Chez Scheme
;
; $Id$

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

(define (assemble313 file)
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


(define (disassemble-file infile)

  (define (print-constvector cv)
    (let loop ((i 0))
      (if (< i (vector-length cv))
	  (begin
	    (case (car (vector-ref cv i))
	      ((codevector)
	       (print-ilist (disassemble (cadr (vector-ref cv i)))))
	      ((constantvector)	
	       (print-constvector (cadr (vector-ref cv i)))
	       (display "-----")))
	    (loop (+ i 1))))))

  (let ((p (open-input-file infile)))
    (let ((segment (read p)))
      (print-ilist (disassemble (car segment)))
      (display "-----")
      (newline)
      (print-constvector (cdr segment)))))

(define (display-in-base n m precision)
  (let ((v '#(0 1 2 3 4 5 6 7 8 9 #\A #\B #\C #\D #\E #\F)))
    (if (zero? n)
	(if (zero? precision)
	    '()
	    (begin (display 0)
		   (display-in-base n m (- precision 1))))
	(begin (display-in-base (quotient n m) m (- precision 1))
	       (display (vector-ref v (remainder n m)))))))

(define (disassemble313 file . rest)
  (define (dis313 q)
    (call-with-input-file
     file
     (lambda (p)
       (newline q)
       (issue-warnings #f)
       (do ((x (read p) (read p)))

; Macros used in the MacScheme version to reduce the size of the heap image.

(define-macro optimize
  (lambda (l)
    (list 'declare
          (list 'optimize
                (cond ((and (memq 'safety l) (memq 'speed l)) 2)
                      ((memq 'speed l) 3)
                      ((memq 'space l) 1)
                      ((memq 'safety l) 1)
                      (else (optimization)))))))

(define-macro optimize
  (lambda (l)
    '(declare (optimize 1)))) ; for now
