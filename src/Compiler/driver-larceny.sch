; Copyright 1998, 2000 Lars T Hansen.
; 
; $Id$
;
; 2000-09-26 / lth
;
; Compilation parameters and driver procedures -- for Larceny's standard
; heap (twobit exposed only through COMPILE-FILE and COMPILE-EXPRESSION).
; Uses basis functionality defined in driver-common.sch

(define (make-file-processer/preserve-reader-state a-process-file)
  ;; FIXME: The reader modes are parameterized here to protect the
  ;; interactive session's modes from changes made while reading the
  ;; compiled file.
  ;; FIXME: This needs to be kept in sync with the preserved
  ;; parameters in src/Lib/Common/load.sch until we adopt a more
  ;; robust solution.
  (lambda args
    (parameterize ((recognize-keywords?          (recognize-keywords?))
                   (recognize-javadot-symbols?   (recognize-javadot-symbols?))
                   (read-square-bracket-as-paren (read-square-bracket-as-paren))
                   (case-sensitive?              (case-sensitive?))
                   (read-r6rs-flags?             #t)
                   (read-larceny-weirdness?      (read-larceny-weirdness?))
                   (read-traditional-weirdness?  (read-traditional-weirdness?))
                   (read-mzscheme-weirdness?     (read-mzscheme-weirdness?)))
      (apply a-process-file args))))

(define process-file/preserve-reader-state 
  (make-file-processer/preserve-reader-state process-file))
(define process-file-block/preserve-reader-state
  (make-file-processer/preserve-reader-state process-file-block))

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
            (process-file-block/preserve-reader-state
                                infilename
                                `(,outfilename binary)
                                (cons write-fasl-token
                                      (assembly-declarations user))
                                dump-fasl-segment-to-port
                                (lambda (forms)
                                  (assemble (compile-block forms syntaxenv) 
                                            user)))
            (process-file/preserve-reader-state
                          infilename
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


; Compile and assemble a single expression; return the LOP segment.

(define (compile-expression expr env)
  (assemble 
   (compile expr (environment-syntax-environment env))))


; Macro-expand a single expression; return the expanded form.

(define (macro-expand-expression expr env)
  (make-readable 
   (expand expr (environment-syntax-environment env))))

;;; The procedures compile313 assemble313 and compile-and-assemble313
;;; might not be appropriate for "real" Larceny systems, which is what
;;; this file is supposed to be "driving."  
;;;
;;; However, Common Larceny is currently built on top of compile313,
;;; and we want it to use the appropriate syntactic environment.  So
;;; in the short term, we'll define compile313 et al. in this driver
;;; file to be like the ones in the driver-twobit file, except that 
;;; these use the host's syntax environment.
;;;
;;; (This whole issue of "where to get the syntax environment from"
;;;  _should_ become irrelevant with R6RS libraries, Felix hopes.)

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
    (let ((syntaxenv (syntactic-copy
                      (environment-syntax-environment
                       (interaction-environment)))))
      (if (benchmark-block-mode)
          (process-file-block/preserve-reader-state
                              infilename 
			      outfilename 
			      '()
			      write-lap 
                              (lambda (x)
                                (compile-block x syntaxenv)))
          (process-file/preserve-reader-state
                        infilename 
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
    (process-file/preserve-reader-state
                  file
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
    (let ((syntaxenv (syntactic-copy
                      (environment-syntax-environment 
                       (interaction-environment)))))
      (if (benchmark-block-mode)
          (process-file-block/preserve-reader-state
                              input-file
                              `(,output-file binary)
			      (assembly-declarations user)
                              write-lop
                              (lambda (x)
				(assemble (compile-block x syntaxenv) user)))
          (process-file/preserve-reader-state
                        input-file
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
      (process-file/preserve-reader-state
                    `(,infilename binary)
                    `(,outfilename binary)
		    (list write-fasl-token)
                    dump-fasl-segment-to-port
                    (lambda (x) x))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Make-fasl not supported on this target architecture.")
      (doit)))


; eof
