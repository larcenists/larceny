; A thought experiment.

(define install-debugger)
(define uninstall-debugger)

(let ((old-evaluator #f))
  (set! install-debugger
        (lambda ()
          (if old-evaluator
              (uninstall-debugger))
          (set! old-evaluator (evaluator))
          (evaluator
           (lambda (expr env)
             (call-with-handler 
              (lambda (exn)
                (let ((msg (exception-message exn)))
                  (display msg)
                  (newline)
                  (debug/enter-debugger (exception-continuable? exn))
                  (when (not (exception-continuable? exn))
                    (display "Internal error in debugger -- it returned.")
                    (newline)
                    (exit))))
              (lambda ()
                (old-evaluator expr env)))))))

  (set! uninstall-debugger
        (lambda ()
          (if (not old-evaluator)
              (error "Debugger not installed."))
          (evaluator old-evaluator)
          (set! old-evaluator #f))))

; eof
