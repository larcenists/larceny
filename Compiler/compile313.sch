; Copyright 1998 Lars T Hansen.
; 
; $Id$
;
; 28 April 1999
;
; compile313 -- compilation parameters and driver procedures.

; File types -- these may differ between operating systems.

(define *scheme-file-types* '(".sch" ".scm"))
(define *lap-file-type*     ".lap")
(define *mal-file-type*     ".mal")
(define *lop-file-type*     ".lop")
(define *fasl-file-type*    ".fasl")

; Compile and assemble a scheme source file and produce a fastload file.

(define (compile-file infilename . rest)

  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename
                                  *scheme-file-types*
                                  *fasl-file-type*)))
          (user
           (assembly-user-data)))
      (if (and (eq? (integrate-procedures) 'none)
               (issue-warnings))
          (begin 
            (display "WARNING from compiler: ")
            (display "integrate-procedures = none")
            (newline)
            (display "Performance is likely to be poor.")
            (newline)))
      (let ((syntaxenv
             (syntactic-copy
              (environment-syntax-environment
               (interaction-environment)))))
        (if (benchmark-block-mode)
            (process-file-block infilename
                                outfilename
                                dump-fasl-segment-to-port
                                (lambda (forms)
                                  (assemble (compile-block forms syntaxenv) 
                                            user)))
            (process-file infilename
                          outfilename
                          dump-fasl-segment-to-port
                          (lambda (expr)
                            (assemble (compile expr syntaxenv) user)))))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Compile-file not supported on this target architecture.")
      (doit)))


; Assemble a MAL or LOP file and produce a FASL file.

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
                    outfilename
                    dump-fasl-segment-to-port
                    (lambda (x) (assemble (if malfile? (eval x) x) user)))
      (unspecified)))
  
  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Assemble-file not supported on this target architecture.")
      (doit)))


; Compile and assemble a single expression; return the LOP segment.

(define (compile-expression expr env)
  (assemble 
   (compile expr (environment-syntax-environment env))))

(define (macro-expand-expression expr env)
  (make-readable 
   (macro-expand expr (environment-syntax-environment env))))

; Compile a scheme source file to a LAP file.

(define (compile313 infilename . rest)
  (let ((outfilename
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type infilename
                                *scheme-file-types* 
                                *lap-file-type*)))
        (write-lap
         (lambda (item port)
           (write item port)
           (newline port)
           (newline port))))
    (let ((syntaxenv
           (syntactic-copy
            (environment-syntax-environment
             (interaction-environment)))))
      (if (benchmark-block-mode)
          (process-file-block infilename outfilename write-lap 
                              (lambda (x)
                                (compile-block x syntaxenv)))
          (process-file infilename outfilename write-lap 
                        (lambda (x)
                          (compile x syntaxenv)))))
    (unspecified)))


; Assemble a LAP or MAL file to a LOP file.

(define (assemble313 file . rest)
  (let ((outputfile
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type file 
                                (list *lap-file-type* *mal-file-type*)
                                *lop-file-type*)))
        (malfile?
         (file-type=? file *mal-file-type*))
        (user
         (assembly-user-data)))
    (process-file file
                  outputfile
                  write-lop
                  (lambda (x) (assemble (if malfile? (eval x) x) user)))
    (unspecified)))


; Compile and assemble a Scheme source file to a LOP file.

(define (compile-and-assemble313 input-file . rest)
  (let ((output-file
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type input-file 
                                *scheme-file-types*
                                *lop-file-type*)))
        (user
         (assembly-user-data)))
    (let ((syntaxenv
           (syntactic-copy
            (environment-syntax-environment
             (interaction-environment)))))
      (if (benchmark-block-mode)
          (process-file-block input-file
                              output-file
                              write-lop
                              (lambda (x) (assemble (compile-block x syntaxenv)
                                                    user)))
          (process-file input-file
                        output-file
                        write-lop
                        (lambda (x) (assemble (compile x syntaxenv) user)))))
    (unspecified)))


; Convert a LOP file to a FASL file.

(define (make-fasl infilename . rest)
  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename
                                  *lop-file-type*
                                  *fasl-file-type*))))
      (process-file infilename
                    outfilename
                    dump-fasl-segment-to-port
                    (lambda (x) x))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Make-fasl not supported on this target architecture.")
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
        ((null? (cdr rest))
         (case (car rest)
           ((0 slow)             (slow-code))
           ((1 standard)         (standard-code))
           ((2 fast-safe)        (fast-safe-code))
           ((3 fast-unsafe)      (fast-unsafe-code))
           ((default
             factory-settings)   (fast-safe-code)
                                 (include-source-code #t)
                                 (benchmark-mode #f)
                                 (benchmark-block-mode #f)
                                 (common-subexpression-elimination #f)
                                 (representation-inference #f))
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
         (if (not (eq? (car args) 'ignore))
             ((car args)))
         (car args))
        (else
         (display "Error: incorrect arguments to compiler-flags")
         (newline)
         (reset))))


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

; Same as above, but passes a list of the entire file's contents
; to the processer.
; FIXME:  Both versions of PROCESS-FILE always delete the output file.
; Shouldn't it be left alone if the input file can't be opened?

(define (process-file-block infilename outfilename writer processer)
  (delete-file outfilename)
  (call-with-output-file outfilename
    (lambda (outport)
      (call-with-input-file infilename
        (lambda (inport)
          (do ((x (read inport) (read inport))
               (forms '() (cons x forms)))
              ((eof-object? x)
               (writer (processer (reverse forms)) outport))))))))

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
