(define (make-engine thunk)
  (call-with-current-continuation
   (lambda (exit)
     (let-values ((here ticks))
       (call-with-current-continuation
        (lambda (there)
          (exit (lambda (ticks complete expire)
                  (call-with-current-continuation
                   ((lambda (here)
                      (there here ticks))
                    complete
                    expire))))))
       (parameterize ((timer-interrupt-handler
                       (lambda ()
                         (enable-interrupts (standard-timeslice))
                         (here (lambda (complete expire)
                                 (expire (make-engine
                                          (lambda ()...))))))
         (call-with-values
          thunk
          (lambda results
            (here (lambda (complete expire)
                    (apply complete 0 results))))))))))

