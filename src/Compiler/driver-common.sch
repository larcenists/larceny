; Copyright 1998, 2000 Lars T Hansen.
; 
; $Id$
;
; 2000-09-27 / lth
;
; Compilation parameters and driver procedures -- common defintions.
; Additional definitions are in driver-larceny.sch and driver-twobit.sch.
;
; Interface in development system
;   COMPILE-FILE
;   ASSEMBLE-FILE, defined below
;   COMPILE313
;   ASSEMBLE313
;   COMPILE-AND-ASSEMBLE313
;   MAKE-FASL
;   DISASSEMBLE, defined below
;   DISASSEMBLE-FILE, defined below
;   Sundry compiler switches and switch management procedures
;
; Interface in standard system
;   COMPILE-FILE
;   ASSEMBLE-FILE, defined below
;   COMPILE-EXPRESSION
;   MACRO-EXPAND-EXPRESSION
;   DISASSEMBLE, defined below
;   DISASSEMBLE-FILE, defined below
;   Sundry compiler switches and switch management procedures, defined below

; File types -- these may differ between operating systems.

(define *scheme-file-types* '(".sch" ".scm"))
(define *lap-file-type*     ".lap")
(define *mal-file-type*     ".mal")
(define *lop-file-type*     ".lop")
(define *fasl-file-type*    ".fasl")


; Write the token #!fasl at the top of a fasl file.

(define (write-fasl-token outport)
  (for-each (lambda (char)
              (write-char char outport))
            (string->list "#!fasl"))
  (newline outport))

; Write a declaration to an output port

(define (process-decl outport)
  (lambda (decl)
    (cond
      ((procedure? decl)
       (decl outport))
      (else
       (write decl outport)
       (newline outport)))))

; Assemble a MAL or LAP file and produce a FASL file.

(define (assemble-file infilename . rest)
  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename 
                                  (list *lap-file-type* *mal-file-type*)
                                  *fasl-file-type*)))
          (malfile?
           (file-type=? infilename *mal-file-type*))
          (user
           (assembly-user-data)))
      (process-file infilename
                    `(,outfilename binary)
		    (cons write-fasl-token
                          (assembly-declarations user))
                    dump-fasl-segment-to-port
                    (lambda (x) (assemble (if malfile? (eval x) x) user)))
      (unspecified)))
  
  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Assemble-file not supported on this target architecture.")
      (doit)))


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
                                (cadr (vector-ref cv i)))
                               port))
          ((constantvector)	
           (print "Constant vector")
           (print-constvector (cadr (vector-ref cv i))))
          ((global)
           (print "Global: " (cadr (vector-ref cv i))))
          ((data)
           (print "Data: " (cadr (vector-ref cv i)))))))
  
  (define (print-segment segment)
    (print "Segment # " segment-no)
    (print-instructions (disassemble-codevector (car segment)) port)
    (print-constvector (cdr segment))
    (print "========================================"))
  
  (cond ((procedure? item)
         (print-instructions (disassemble-codevector (procedure-ref item 0))
                             port))
        ((and (pair? item)
              (bytevector? (car item))
              (vector? (cdr item)))
         (print-segment item))
        (else
         (error "disassemble-item: " item " is not disassemblable."))))


; Disassemble a ".lop" or ".fasl" file; dump output to screen or 
; other (optional) file.

(define (disassemble-file file . rest)
  
  (define (doit input-port output-port)
    (display "; From " output-port)
    (display file output-port)
    (newline output-port)
    (do ((segment-no 0 (+ segment-no 1))
         (segment (read input-port) (read input-port)))
        ((eof-object? segment))
        (disassemble-item segment segment-no output-port)))

  ; disassemble313

  (call-with-input-file
   file
   (lambda (input-port)
     (if (null? rest)
         (doit input-port (current-output-port))
         (begin
          (delete-file (car rest))
          (call-with-output-file
           (car rest)
           (lambda (output-port) (doit input-port output-port)))))))
  (unspecified))


; Display and manipulate the compiler switches.

(define (compiler-switches . rest)

  (define (slow-code)
    (set-compiler-flags! 'no-optimization)
    (set-assembler-flags! 'no-optimization))

  (define (standard-code)
    (set-compiler-flags! 'standard)
    (set-assembler-flags! 'standard))

  (define (fast-safe-code)
    (set-compiler-flags! 'fast-safe)
    (set-assembler-flags! 'fast-safe))

  (define (fast-unsafe-code)
    (set-compiler-flags! 'fast-unsafe)
    (set-assembler-flags! 'fast-unsafe))

  (cond ((null? rest)
         (display "Debugging:")
         (newline)
         (display-twobit-flags 'debugging)
         (display-assembler-flags 'debugging)
         (newline)
         (display "Safety:")
         (newline)
         (display-twobit-flags 'safety)
         (display-assembler-flags 'safety)
         (newline)
         (display "Speed:")
         (newline)
         (display-twobit-flags 'optimization)
         (display-assembler-flags 'optimization)
         (if #f #f))
	((eq? (car rest) 'get)
	 (let ((t (twobit-all-flags))
	       (a (assembler-all-flags)))
	   (lambda ()
	     (t)
	     (a))))
	((eq? (car rest) 'set!)
	 ((cadr rest)))
        ((null? (cdr rest))
         (case (car rest)
           ((0 slow)             (slow-code))
           ((1 standard)         (standard-code))
           ((2 fast-safe)        (fast-safe-code))
           ((3 fast-unsafe)      (fast-unsafe-code))
           ((default
             factory-settings)   (fast-safe-code)
                                 (benchmark-block-mode #f))
           (else 
            (error "Unrecognized flag " (car rest) " to compiler-switches.")))
         (unspecified))
        (else
         (error "Too many arguments to compiler-switches."))))

; Meta-switches
; Accept a previously returned value as well as the symbol IGNORE, which 
; is ignored and facilitates use with PARAMETERIZE.

(define (global-optimization-flags . args)
  (cond ((null? args)
         (let ((t.g.o (twobit-global-optimization-flags))
               (a.g.o (assembler-global-optimization-flags)))
           (lambda ()
             (t.g.o)
             (a.g.o))))
        ((null? (cdr args))
         (if (not (eq? (car args) 'ignore))
             ((car args)))
         (car args))
        (else
         (display "Error: incorrect arguments to global-optimization-flags")
         (newline)
         (reset))))

(define (runtime-safety-flags . args)
  (cond ((null? args)
         (let ((t.r.s (twobit-runtime-safety-flags))
               (a.r.s (assembler-runtime-safety-flags)))
           (lambda ()
             (t.r.s)
             (a.r.s))))
        ((null? (cdr args))
         (if (not (eq? (car args) 'ignore))
             ((car args)))
         (car args))
        (else
         (display "Error: incorrect arguments to runtime-safety-flags")
         (newline)
         (reset))))
                 
(define (compiler-flags . args)
  (cond ((null? args)
         (let ((t.a.f (twobit-all-flags))
               (a.a.f (assembler-all-flags)))
           (lambda ()
             (t.a.f)
             (a.a.f))))
        ((null? (cdr args))
         (case (car args)
           ((ignore) #t)
           ((0 1 2 3
             slow standard fast-safe fast-unsafe default factory-settings)
            (compiler-switches (car args)))
           (else
            (if (procedure? (car args))
                ((car args))
                (begin
                  (display "Error: incorrect arguments to compiler-flags")
                  (newline)
                  (reset)))))
         (car args))
        (else
         (display "Error: incorrect arguments to compiler-flags")
         (newline)
         (reset))))


; Read and process one file, producing another.
; Filenames can be simple strings or list (filename mode) where mode
; is a symbol, "text" or "binary".

(define (process-file infilename outfilename decls writer processer)
  (process-files (list infilename) outfilename decls writer processer))

(define (process-files infilenames outfilename decls writer processer)
  (let ((outfilename (if (pair? outfilename) (car outfilename) outfilename))
	(outfilefn   (if (and (pair? outfilename) 
			      (eq? 'binary (cadr outfilename)))
			 call-with-raw-latin-1-output-file
			 call-with-output-file)))
    (define (attempt-compilation)
      (outfilefn outfilename
                 (lambda (outport)
                   (for-each (process-decl outport) decls)
                   (for-each
                    (lambda (infilename)
                      (let ((infilename  (if (pair? infilename) 
                                             (car infilename) 
                                             infilename))
                            (infilefn    (if (and (pair? infilename)
                                                  (eq? 'binary
                                                       (cadr infilename)))
                                             call-with-raw-latin-1-input-file
                                             call-with-input-file)))
                        (infilefn infilename
                                  (lambda (inport)
                                    (do ((x (read inport) (read inport)))
                                        ((eof-object? x))
                                      (writer (processer x) outport))))))
                    infilenames))))
    
    (delete-file outfilename)
    (let ((compilation-complete #f))
      (dynamic-wind
          (lambda () 
            (cond (compilation-complete
                   (error "Attempted to resume an abandoned compilation."))))
          (lambda () (attempt-compilation) (set! compilation-complete #t))
          (lambda () 
            (cond ((not compilation-complete)
                   (delete-file outfilename)
                   (set! compilation-complete #t))))))))

; Same as above, but passes a list of the entire file's contents
; to the processer.  Note, processes one input file at a time, though
; plausibly it should read all the files and process the collected
; input together.
;
; FIXME:  Both versions of PROCESS-FILE always delete the output file.
; Shouldn't it be left alone if the input file can't be opened?

(define (process-file-block infilename outfilename decls writer processer)
  (process-files-block (list infilename) outfilename decls writer processer))

(define (process-files-block infilenames outfilename decls writer processer)
  (let ((outfilename (if (pair? outfilename) (car outfilename) outfilename))
	(outfilefn   (if (and (pair? outfilename) 
			      (eq? 'binary (cadr outfilename)))
			 call-with-raw-latin-1-output-file
			 call-with-output-file)))
    (delete-file outfilename)
    (outfilefn outfilename
      (lambda (outport)
	(for-each (process-decl outport) decls)
	(for-each
	 (lambda (infilename)
	   (let ((infilename  (if (pair? infilename) 
				  (car infilename)
				  infilename))
		 (infilefn    (if (and (pair? infilename)
				       (eq? 'binary (cadr infilename)))
				  call-with-raw-latin-1-input-file
				  call-with-input-file)))
	     (infilefn infilename
		       (lambda (inport)
			 (do ((x (read inport) (read inport))
			      (forms '() (cons x forms)))
			     ((eof-object? x)
			      (writer (processer (reverse forms)) 
				      outport)))))))
	 infilenames)))))

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
                   (if (file-type=? filename n)
                       (string-append (substring filename 0 (- j l)) new)
                       (loop (cdr m))))))))))

(define (file-type=? file-name type-name)
  (let ((fl (string-length file-name))
        (tl (string-length type-name)))
    (and (>= fl tl)
         (string-ci=? type-name
                      (substring file-name (- fl tl) fl)))))

; eof
