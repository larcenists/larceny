(load "Twobit.fasl")
(load "Compiler/driver-larceny.sch")
(load "Util/seal-twobit.sch")
(load "Util/dotnet-compile-file.sch")
(load "Asm/IL/il-load-coremem.sch")

(seal-twobit 
 (append 

  ;; From Asm/IL/il-corememory.sch
  '(link-lop-segment/clr 
    eval/clr 
    load-lop/clr 
    with-saving-assembly-to-dll/full-control 
    with-saving-assembly-to-dll 
    compile-file/clr) 
  
  standard-proc-names))

(begin
  (load "Debugger/debug.sch")
  (load "Debugger/inspect-cont.sch")
  (load "Debugger/trace.sch")
  (install-debugger))

; It's necessary to set the interaction environment so that any uses of 
; EVAL in the loaded file will reference the correct environment.

(define new-load-eval
  (lambda (expr env)
    (let ((old-env (interaction-environment)))
      (define (literal? x)
        (or (procedure? x) (number? x) (string? x)))
      (dynamic-wind 
          (lambda ()
            (interaction-environment env))
          (lambda ()
            ;; Filter out procedure literals (.fasl files)
            ;; Keep in sync w/ dump-fasl in Asm/IL/dumpheap-extra.sch
            (if (and (pair? expr) 
                     (pair? (car expr))
                     (eq? '.common-patch-procedure (caar expr))
                     (every? literal? (cdar expr)))
                (let ((proc
                       (apply (eval '.common-patch-procedure env)
                              (cdar expr))))
                  (proc))
                (eval/clr expr env)))
          (lambda ()
            (if (eq? (interaction-environment) env)
                (interaction-environment old-env)))))))
