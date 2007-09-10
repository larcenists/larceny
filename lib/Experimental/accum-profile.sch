;; exports:
;; make-accum-profile 
;; accum-profile-times
;; instrument-accumulating-time!
;; remove-instrumentation-from-accum-profile!

;; This uses debug/wrap-procedure to intercept calls to PROC and
;; temporarily divert them to the instrumenting code
(require 'trace)

(define accum-profile-rt 
  (make-record-type "accum-profile" '(times wrap-records) #f))

;; make-accum-profile : () -> AccumProfile
(define make-accum-profile
  (let ((ctor (record-constructor accum-profile-rt)))
    (lambda ()
      (ctor '() '()))))

(define accum-profile-times
  (record-accessor accum-profile-rt 'times))

;; instrument-accumulating-time! : AccumProfile Symbol Procedure -> unspecified
(define instrument-accumulating-time!
  (let ((ensure-entry-exists!
         (let ((set-times! (record-updater accum-profile-rt 'times)))
           (lambda (accum-profile name)
             (let ((times (accum-profile-times accum-profile)))
               (cond ((not (assq name times))
                      (set-times! accum-profile
                                  (cons (list name 'elapsed: 0 'user: 0 'system: 0) times))))))))

        (add-wrap-record!
         (let ((wrap-records (record-accessor accum-profile-rt 'wrap-records))
               (set-wrap-records! (record-updater accum-profile-rt 'wrap-records)))
           (lambda (accum-profile wrap-record)
             (set-wrap-records! accum-profile 
                                (cons wrap-record (wrap-records accum-profile))))))

        (accumulate-time! 
         (lambda (accum-profile name elapsed user system)
           (let* ((times (accum-profile-times accum-profile))
                  (entry (assq name times)))
             (set-cdr! entry (list 'elapsed: (+ elapsed (list-ref entry 2))
                                   'user:    (+ user    (list-ref entry 4))
                                   'system:  (+ system  (list-ref entry 6)))))))
        (last-memstats*updater #f)
        
        )
    
    (lambda (accum-profile name proc)
      (ensure-entry-exists! accum-profile name)
      (let ((wrap-record
             (debug/wrap-procedure
              proc
              (lambda (compute)
                (let ((start-memstats #f))
                  (lambda args
                    (dynamic-wind
                        ;; XXX not the right thing for recursive
                        ;; procedures, (or nesting in general); would
                        ;; be nice to determine when control flows
                        ;; from one instrumented procedure into
                        ;; another and account accordingly...
                        (lambda () (set! start-memstats (memstats)))
                        (lambda () (apply compute args))
                        (lambda () (let* ((finis-memstats (memstats))
                                          (elapsed (- (memstats-elapsed-time finis-memstats)
                                                      (memstats-elapsed-time start-memstats)))
                                          (system (- (memstats-system-time finis-memstats)
                                                     (memstats-system-time start-memstats)))
                                          (user (- (memstats-user-time finis-memstats)
                                                   (memstats-user-time start-memstats))))
                                     (accumulate-time! accum-profile name elapsed user system))))))))))
        (add-wrap-record! accum-profile wrap-record)))))

(define remove-instrumentation-from-accum-profile! 
  (let ((wrap-records (record-accessor accum-profile-rt 'wrap-records))
        (set-wrap-records! (record-updater accum-profile-rt 'wrap-records)))
    (lambda (accum-profile)
      (for-each (lambda (wrap-record)
                  (debug/undo-wrapping wrap-record))
                (wrap-records accum-profile))
      (set-wrap-records! accum-profile '()))))
