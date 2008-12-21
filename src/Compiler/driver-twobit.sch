; Copyright 1998, 2000 Lars T Hansen.
; 
; $Id$
;
; 2000-09-26 / lth
;
; Compilation parameters and driver procedures -- for the development
; environment (twobit.heap and the build script).  Uses basis functionality
; defined in driver-common.sch

; FIXME:  This is here so it will be loaded during bootstrapping
; and present in development heaps.  For released heaps, it is
; defined in src/Lib/Common/timer.sch (for the time being).

(define twobit-timer-hook
  (make-parameter "twobit-timer-hook" #f))

; Compile and assemble a scheme source file and produce a FASL file.

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
      (if (eq? (integrate-procedures) 'none)
          (twobit-warn
           (string-append
            "integrate-procedures = none"
            (string #\newline)
            "Performance is likely to be poor.")))
      (let ((syntaxenv (syntactic-copy (the-usual-syntactic-environment))))
        (if (benchmark-block-mode)
            (process-file-block infilename
                                `(,outfilename binary)
                                (cons write-fasl-token
                                      (assembly-declarations user))
                                dump-fasl-segment-to-port
                                (lambda (forms)
                                  (assemble (compile-block forms syntaxenv) 
                                            user)))
            (process-file infilename
                          `(,outfilename binary)
                          (cons write-fasl-token
                                (assembly-declarations user))
                          dump-fasl-segment-to-port
                          (lambda (expr)
                            (assemble (compile expr syntaxenv) user)))))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Compile-file not supported on this target architecture.")
      (doit)))


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
    (let ((syntaxenv (syntactic-copy (the-usual-syntactic-environment))))
      (if (benchmark-block-mode)
          (process-file-block infilename 
			      outfilename 
			      '()
			      write-lap 
                              (lambda (x)
                                (compile-block x syntaxenv)))
          (process-file infilename 
			outfilename 
			'()
			write-lap 
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
                  `(,outputfile binary)
		  (assembly-declarations user)
                  write-lop
                  (lambda (x) 
		    (assemble (if malfile? (eval x) x) user)))
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
    (let ((syntaxenv (syntactic-copy (the-usual-syntactic-environment))))
      (if (benchmark-block-mode)
          (process-file-block input-file
                              `(,output-file binary)
			      (assembly-declarations user)
                              write-lop
                              (lambda (x)
				(assemble (compile-block x syntaxenv) user)))
          (process-file input-file
                        `(,output-file binary)
			(assembly-declarations user)
                        write-lop
                        (lambda (x) 
			  (assemble (compile x syntaxenv) user)))))
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
      (process-file `(,infilename binary)
                    `(,outfilename binary)
		    (list write-fasl-token)
                    dump-fasl-segment-to-port
                    (lambda (x) x))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Make-fasl not supported on this target architecture.")
      (doit)))


; eof
