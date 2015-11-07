;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme process-context) procedures:
;;;
;;;     command-line
;;;     emergency-exit
;;;     exit
;;;     get-environment-variable
;;;     get-environment-variables


(define-library (tests scheme process-context)
  (export run-process-context-tests)
  (import (scheme base)
          (scheme process-context)
          (tests scheme test))

  (import (scheme write)) ; FIXME

  (begin

   (define cmds '())

   (define (run-process-context-tests)

     (test/unspec (set! cmds (command-line)))

     (test (<= 1 (length cmds))
           #t)

     (let ((probe (member "--test-getenv" cmds)))
       (if probe
           (let ((var (cadr probe))
                 (val (car (cddr probe))))
             (test (get-environment-variable var)
                   val)
             (test (let* ((alist (get-environment-variables))
                          (probe (assoc var alist)))
                     (and probe (cdr probe)))
                   val))))

     (let* ((probe (member "--test-emergency-exit" cmds))
            (exit-status (and probe
                              (pair? (cdr probe))
                              (string->number (car (cdr probe))))))
       (if probe
           (test (dynamic-wind
                  (lambda () #t)
                  (if exit-status
                      (lambda () (emergency-exit exit-status))
                      emergency-exit)
                  (lambda ()
                    (test "shouldn't get here" #f)
                    (report-test-results)))
                 "irrelevant")))

     (let* ((probe (member "--test-exit" cmds))
            (exit-status (and probe
                              (pair? (cdr probe))
                              (string->number (car (cdr probe))))))
       (if probe
           (test (if exit-status
                     (exit exit-status)
                     (exit))
                 "shouldn't get here")))

     )))

                 
        
        