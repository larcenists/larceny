; Copyright 1998 Lars T Hansen.
; 
; $Id$
;
; compile313 -- compilation parameters and driver procedures.

; File types -- these may differ between operating systems.

(define *scheme-file-types* '(".sch" ".scm"))
(define *lap-file-type*     ".lap")
(define *mal-file-type*     ".mal")
(define *lop-file-type*     ".lop")
(define *fasl-file-type*    ".fasl")

;;; Driver procedurs

; Compile and assemble a scheme source file and produce a fastload file.

(define (compile-file infilename . rest)
  (let ((outfilename
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type infilename
                                *scheme-file-types*
                                *fasl-file-type*)))
        (user
         (assembly-user-data)))
    (process-file infilename
                  outfilename
                  dump-fasl-segment-to-port
                  (lambda (expr)
                    (assemble (compile expr) user)))
    (unspecified)))


; Assemble a MAL or LOP file and produce a FASL file.

(define (assemble-file infilename . rest)
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


; Compile and assemble a single expression; return the LOP segment.

(define (compile-and-assemble-expression expr)
  (assemble (compile expr)))


; Compile a scheme source file to a LAP file.

(define (compile313 file . rest)
  (let ((outputfile
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type file
                                *scheme-file-types* 
                                *lap-file-type*))))
    (process-file file
                  outputfile
                  (lambda (item port)
                    (write item port)
                    (newline port)
                    (newline port))
                  (lambda (x)
                    (compile x)))
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

; Convert a LOP file to a FASL file.

(define (make-fasl infilename . rest)
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


; Quick ways of setting compiler and assembler switches.

(define (slow-code)
  (set-compiler-flags! 'no-optimization)
  (set-assembler-flags! 'no-optimization))

(define (default-code)
  (set-compiler-flags! 'default)
  (set-assembler-flags! 'default))

(define (fast-safe-code)
  (set-compiler-flags! 'fast-safe)
  (set-assembler-flags! 'fast-safe))

(define (fast-unsafe-code)
  (set-compiler-flags! 'fast-unsafe)
  (set-assembler-flags! 'fast-unsafe))


; Display all flags

(define (compiler-switches)
  (display-twobit-flags)
  (newline)
  (display-assembler-flags)
  (unspecified))


; Read and process one file, producing another.
; Preserves the global syntactic environment.

(define (process-file infilename outfilename writer processer)
  (define (doit)
    (delete-file outfilename)
    (call-with-output-file
     outfilename
     (lambda (outport)
       (call-with-input-file
        infilename
        (lambda (inport)
          (let loop ((x (read inport)))
            (if (eof-object? x)
                #t
                (begin (writer (processer x) outport)
                       (loop (read inport))))))))))
  (let ((current-syntactic-environment
         global-syntactic-environment))
    (dynamic-wind
     (lambda ()
       (set! global-syntactic-environment
             (make-extended-syntactic-environment)))
     (lambda () (doit))
     (lambda ()
       (set! global-syntactic-environment
             current-syntactic-environment)))))


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
