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

(define (load filename . rest)

  (define (get-environment)
    (cond ((null? rest)
           (interaction-environment))
          ((null? (cdr rest))
           (car rest))
          (else
           (error "load: too many arguments")
           #t)))

  ;; The environment must be recomputed for each expression evaluation --
  ;; the loaded expressions may change the interaction environment, and
  ;; when the environment is implicit, that change should be reflected in
  ;; subsequent evaluations.

  (define (load-file)
    (let ((p (open-binary-input-file filename)))
      (if (load-verbose)
          (begin
            (newline (current-output-port))
            (write-string "; Loading " (current-output-port))
            (display filename (current-output-port))
            (flush-output-port (current-output-port))))
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
      (close-input-port p)
      (unspecified)))

  ;; The linker is implicit in the loader (as the #^G thing) and uses
  ;; global-name-resolver as the linker.  The following hack makes
  ;; sure that global-name-resolver uses the right environment.  We
  ;; need to separate linking from loading, which will remove this
  ;; silliness.
  ;;
  ;; FIXME:  The reader modes are parameterized here to protect
  ;; the interactive session's modes from changes made by the
  ;; loaded file.  As Felix points out, that makes it impossible
  ;; for an initialization file to set a user's preferred reader
  ;; modes.  The right way to do this is to parameterize around
  ;; the input of an entire file, and to evaluate the forms in
  ;; the file only after the reader mode has been restored.
  ;; Then an initialization file could change the interactive
  ;; session's modes by setting parameters directly, but flags
  ;; such as #!r6rs would not corrupt the interactive session.
  ;; I'm afraid to do that for v0.94, however, because Larceny
  ;; has been evaluating each form immediately after it is read,
  ;; and there might be some situations for which that matters.

  (parameterize ((global-name-resolver
                  (lambda (sym)
                    (environment-get-cell (get-environment) sym)))
                 (recognize-keywords?          (recognize-keywords?))
                 (recognize-javadot-symbols?   (recognize-javadot-symbols?))
                 (read-square-bracket-as-paren (read-square-bracket-as-paren))
                 (case-sensitive?              (case-sensitive?))
                 (read-r6rs-flags?             #t)
                 (read-larceny-weirdness?      (read-larceny-weirdness?))
                 (read-traditional-weirdness?  (read-traditional-weirdness?))
                 (read-mzscheme-weirdness?     (read-mzscheme-weirdness?)))
    (load-file)))


; List->procedure is used by the reader to deal with #^P.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
          p
          (begin (procedure-set! p i (car l))
                 (loop (cdr l) (+ i 1)))))))

; eof
