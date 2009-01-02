; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- the 'load' procedure.
;
; FIXME:
;  - Not entirely robust, but ok for now.
;  - Loader should install reader macros for #^G, #^B, #^P so that 
;    the reader would not need to be aware of these extensions.

($$trace "load")

; It's necessary to set the interaction environment so that any uses of 
; EVAL in the loaded file will reference the correct environment.

(define load-evaluator
  (make-parameter "load-evaluator"
                  (lambda (expr env)
                    (let ((old-env (interaction-environment)))
                      (dynamic-wind 
                       (lambda ()
                         (interaction-environment env))
                       (lambda ()
                         ;; Filters out procedure literals in input of
                         ;; the form (#^P(...) arg-literals ...)
                         ;; For loading .fasl files when eval compiles
                         ;; rather than interprets.
                         (if (and (pair? expr)
                                  (procedure? (car expr)))
                             (apply (car expr) (cdr expr))
                             (eval expr env)))
                       (lambda ()
                         (if (eq? (interaction-environment) env)
                             (interaction-environment old-env))))))))

(define load-print
  ;; If not #f, print the return value(s) of each form as it is
  ;; loaded.
  (make-parameter "load-print" #f))

(define load-verbose
  ;; If not #f, print the file name before loading.
  (make-parameter "load-verbose" #f))

; The load-from-port procedure has been factored out
; so it could be called after a #!fasl flag is read.
; Note, however, that fasl files must be raw Latin-1.
;
; The second argument is a thunk, usually interaction-environment.

(define (load-from-port p get-environment)

  ;; The environment must be recomputed for each expression evaluation --
  ;; the loaded expressions may change the interaction environment, and
  ;; when the environment is implicit, that change should be reflected in
  ;; subsequent evaluations.

  (do ((expr (read p) (read p)))
      ((eof-object? expr))
    (call-with-values
     (lambda () ((load-evaluator) expr (get-environment)))
     (lambda values
       (if (load-print)
           (for-each (lambda (value)
                       (newline (current-output-port))
                       (write-string ";    " (current-output-port))
                       (write value (current-output-port))
                       (flush-output-port (current-output-port)))
                     values)))))
  (unspecified))

(define (load filename . rest)

  (let ((get-environment
         (cond ((null? rest)
                interaction-environment)
               ((null? (cdr rest))
                (let ((env (car rest)))
                  (lambda () env)))
               (else
                (error "load: too many arguments")
                #t))))

    ;; Fasl files must be raw Latin-1, while source files may be
    ;; UTF-8 or UTF-16 on some platforms.
    ;; To detect fasl files, we open the file as raw Latin-1 and
    ;; look at the first line.  If it's a fasl file, we load it
    ;; as a fasl file.  Otherwise we close the raw Latin-1 port
    ;; and load the file as a source file.
    ;;
    ;; FIXME: this should be kept in sync with lib/R6RS/r6rsmode.sch

    (define (load-file)
      (cond ((call-with-port
              (open-raw-latin-1-input-file filename)
              (lambda (p)
                (if (load-verbose)
                    (begin
                     (newline (current-error-port))
                     (write-string "; Loading " (current-error-port))
                     (display filename (current-error-port))
                     (flush-output-port (current-error-port))))
                (let ((first-line (get-line p)))
                  (cond ((and (string? first-line)
                              (string=? first-line "#!fasl"))
                         (load-from-port p get-environment)
                         #t)
                        (else
                         #f)))))
             (unspecified))
            (else
             (call-with-port
              (open-input-file filename)
              (lambda (p)
                (load-from-port p get-environment))))))

    ;; The linker is implicit in the loader (as the #^G thing) and uses
    ;; global-name-resolver as the linker.  The following hack makes
    ;; sure that global-name-resolver uses the right environment.  We
    ;; need to separate linking from loading, which will remove this
    ;; silliness.

    (parameterize ((global-name-resolver
                    (lambda (sym)
                      (environment-get-cell (get-environment) sym))))
      (load-file))))


; List->procedure is used by the reader to deal with #^P.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
          p
          (begin (procedure-set! p i (car l))
                 (loop (cdr l) (+ i 1)))))))

; eof
