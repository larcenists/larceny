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
    (let ((p (open-input-file filename)))
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
  ;; global-name-resolver as the linker.  The following hacks make
  ;; sure that global-name-resolver uses the right environment.  We
  ;; need to separate linking from loading, which will remove this
  ;; silliness.

  (let ((old-resolver (global-name-resolver))
        (new-resolver (lambda (sym)
                        (environment-get-cell (get-environment) sym))))
    (dynamic-wind 
     (lambda () (global-name-resolver new-resolver))
     (lambda () (load-file))
     (lambda () (global-name-resolver old-resolver)))))


; List->procedure is used by the reader to deal with #^P.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
          p
          (begin (procedure-set! p i (car l))
                 (loop (cdr l) (+ i 1)))))))

; eof
