; Copyright 1998, 2000 Lars T Hansen.
; 
; $Id$
;
; 2000-09-26 / lth
;
; Compilation parameters and driver procedures -- for Larceny's standard
; heap (twobit exposed only through COMPILE-FILE and COMPILE-EXPRESSION).
; Uses basis functionality defined in driver-common.sch

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


; Compile and assemble a single expression; return the LOP segment.

(define (compile-expression expr env)
  (assemble 
   (compile expr (environment-syntax-environment env))))


; Macro-expand a single expression; return the expanded form.

(define (macro-expand-expression expr env)
  (make-readable 
   (expand expr (environment-syntax-environment env))))

; eof
