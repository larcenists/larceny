;; This code requires inspect-cont.sch to be loaded first.

(define (install-profiler! . args)
  (if (not (null? args))
      (set-timer-granularity! (car args)))
  (timer-interrupt-handler 
   (let ((old-tih (timer-interrupt-handler)))
     (lambda l (let* ((el (memstats-elapsed-time (memstats)))
                      (cc (current-continuation-structure)) 
                      (i (make-continuation-inspector cc))
                      (rendered-cont (render-cont i)))
                 (display `(time: ,el cont: ,rendered-cont))
                 (newline)
                 (apply old-tih l))))))

;; The standard timer-interrupt-handler just turns interrupts back on
;; with (standard-timeslice) as the argument; this procedure allows
;; the user to set their own grain.
(define (set-timer-granularity! grain)
  (timer-interrupt-handler
   (lambda () (enable-interrupts grain))))

(define (render-cont i)
  (define (show val cnt)
    (cond ((= cnt 1)
           val)
          (else
           (list val '* cnt))))
  (let loop ((prev (unspecified))
             (pcnt 0))
    (cond ((not (i 'probe-down)) 
           (list (show prev pcnt)))
          (else (i 'down)
                (let* ((info (((i 'get) 'code) 'procedure))
                       (name (cond ((procedure? info) (procedure-name info))
                                   (else info))))
                  (cond ((eq? name prev)
                         (loop name (+ pcnt 1)))
                        ((> pcnt 0)
                         (cons (show prev pcnt) (loop name 1)))
                        (else
                         (loop name 1))))))))

