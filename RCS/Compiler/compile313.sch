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
                    (string-ci=? ".lap" (substring file (- n 4) n)))
               (substring file 0 (- n 4))
               file)
           ".lop")))
    (assemble-file file outputfile)))

(define (assemble-file infile outfile)
  (delete-file outfile)
  (let ((p (open-output-file outfile)))
    (file-for-each (lambda (x) (write (assemble x) p)) infile)
    (close-output-port p)
    #t))

(define (file-for-each proc filename)
  (call-with-input-file infile
    (lambda (p)
      (let loop ((x (read p)))
	(if (eof-object? x)
	    #t
	    (begin (proc x)
		   (loop (read p))))))))

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
           ((eof-object? x)
            (issue-warnings #t))
           (disassemble (assemble x) q)
           (newline q)
           (newline q)))))
  (if (null? rest)
      (dis313 (current-output-port))
      (call-with-output-file (car rest) dis313)))

; Patches to record definitions and uses of global variables.

; (define (begin-link)
;   (set! @globals@ '()))
; 
; (define (end-link)
;   (define (report defined used)
;     (let ((undefined (difference used defined))
;           (unused (difference defined used))
;           (msg1 "Global variables that were used but not defined:")
;           (msg2 "Global variables that were defined but not used:")
;           (show (lambda (name)
;                   (write name)
;                   (newline))))
;       (if (not (null? undefined))
;           (begin (display msg1)
;                  (newline)
;                  (for-each show undefined)
;                  (newline)))
;       (if (not (null? unused))
;           (begin (display msg2)
;                  (newline)
;                  (for-each show unused)))))
;   (set! @globals@
;         (sort @globals@
;               (lambda (x y)
;                 (or (string<? (symbol->string (car x))
;                               (symbol->string (car y)))
;                     (and (eq? (car x) (car y))
;                          (eq? (cadr x) 'defined))))))
;   (do ((globals @globals@ (cdr globals))
;        (defined '() (if (and (eq? (cadr (car globals)) 'defined)
;                              (or (null? defined)
;                                  (not (eq? (car (car globals))
;                                            (car defined)))))
;                         (cons (car (car globals)) defined)
;                         defined))
;        (used '() (if (and (eq? (cadr (car globals)) 'used)
;                           (or (null? used)
;                               (not (eq? (car (car globals))
;                                         (car used)))))
;                      (cons (car (car globals)) used)
;                      used)))
;       ((null? globals)
;        (report (reverse defined) (reverse used)))))

; (define @globals@ '())
; 
; (define (get-value-cell-for-reference symbol)
;   (set! @globals@ (cons (list symbol 'used) @globals@))
;   ; This is the wrong thing.
;   symbol)
; 
; (define (get-value-cell-for-definition symbol)
;   (set! @globals@ (cons (list symbol 'defined) @globals@))
;   ; This is the wrong thing.
;   symbol)
; 
; (define-instruction $global
;   (let ((bits (define-code #x5a 0 0)))
;     (lambda (instruction as)
;       (list-instruction "global" instruction)
;       (let ((vcell (get-value-cell-for-reference (operand1 instruction))))
;         (emit-fixup! as 1 2 (emit-constant as vcell)))
;       (emit! as bits))))

; (define-instruction $setglbl
;   (let ((bits (define-code #x5b 0 0)))
;     (lambda (instruction as)
;       (list-instruction "setglbl" instruction)
;       (let ((vcell (get-value-cell-for-definition (operand1 instruction))))
;         (emit-fixup! as 1 2 (emit-constant as vcell)))
;       (emit! as bits))))

; Macros used in the MacScheme to reduce the size of the heap image.

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
