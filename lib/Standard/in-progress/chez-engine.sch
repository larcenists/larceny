; Work in progress: Engines compatible with Chez Scheme v6
;
; This is wrong: if the engine does not complete, then on subsequent
; invocations, the COMPLETE/EXPIRE that are called are the one in the original
; call, not the ones from the subsequent call.

(define current-engine-stop 
  (make-parameter "current-engine-stop" #f))
  
(define (make-engine thunk)

  (define overhead 10)

  (define (run computation ticks stop return)
    (parameterize ((timer-interrupt-handler
                    (lambda ()
                      (enable-interrupts (standard-timeslice))
                      (stop)))
                   (current-engine-stop
                    (lambda (how values)
                      (case how
                        ((block)  (stop))
                        ((return) (return values))
                        (else ???)))))
      (enable-interrupts (+ ticks overhead))
      (let-values ((vs (thunk)))
        (return vs))))

  (define (create-engine computation)
    (lambda (ticks complete expire)
      (call-with-current-continuation
       (lambda (exit)
         (call-with-current-continuation
          (lambda (outer)
            (let ((stop  
                   (lambda ()
                     (call-with-current-continuation
                      (lambda (new-computation)
                        (outer (lambda (complete expire)
                                 (expire 
                                  (create-engine new-computation))))))))
                  (return 
                   (lambda results
                     (let ((n (disable-interrupts)))
                       (enable-interrupts (standard-timeslice))
                       (outer (lambda (complete expire)
                                (apply complete n results)))))))
              (exit ticks stop return))))
         (computation ticks stop return)))))

  (let-values (((ticks stop return)
                (call-with-current-continuation create-engine)))
    (call-with-values
     (lambda () (run thunk ticks stop return))
     return)))


(define (engine-block)
  ((current-engine-stop) 'block '()))

(define (engine-return . objs)
  ((current-engine-stop) 'return objs))

; eof
