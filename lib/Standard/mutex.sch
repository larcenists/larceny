; Mutex -- single-owner synchronization object.
; 2004-01-18 / lth
;
; MAKE-MUTEX => mutex
;   Create a mutex
;
; MUTEX-ACQUIRE mutex
;   Acquire ownership of the mutex, blocking if it is already held
;   by another thread.  A mutex can be acquired multiple times by 
;   the same thread without being released; if it has been acquired
;   n times, MUTEX-RELEASE must be called n times to relinquish
;   ownership.
;
; MUTEX-RELEASE mutex
;   Reduce by 1 the number of calls to MUTEX-RELEASE needed to
;   relinquish ownership of the mutex.  It is an error to call
;   MUTEX-RELEASE on a mutex not held by the calling task.
;
; MUTEX-HOLDER mutex => obj
;   Returns either #f (nobody is holding it), a task (a task other than
;   the current task is holding it), or a number (the current task holds
;   the mutex with this many releases pending)
;
; MUTEX-WAITERS mutex => number
;   Returns the number of tasks currently blocked on the mutex

(require 'tasking)
(require 'define-record)

(define-record mutex (held count blocked))

(define make-mutex
  (let ((make-mutex make-mutex))
    (lambda ()
      (make-mutex #f 0 '()))))
 
(define (mutex-acquire m)
  (without-interrupts
   (let loop ((h (mutex-held m)))
     (cond ((not h)
            (mutex-count-set! m 1)
            (mutex-held-set! m (current-task)))
           ((eq? h (current-task))
            (mutex-count-set! m (+ 1 (mutex-count m))))
           (else
            (mutex-blocked-set! m (cons (current-task) (mutex-blocked m)))
            (block (current-task))
            (loop (mutex-held m)))))))

(define (mutex-release m)
  (without-interrupts
   (let ((h (mutex-held m)))
     (if (not (eq? h (current-task)))
	 (error "MUTEX-RELEASE: mutex " m " not held by " (current-task))
	 (let ((c (mutex-count m)))
	   (mutex-count-set! m (- c 1))
	   (if (= c 1)
	       (let ((b (mutex-blocked m)))
		 (mutex-blocked-set! m '())
		 (mutex-held-set! m #f)
		 (for-each unblock b))))))))

(define (mutex-holder m)
  (without-interrupts
   (let ((h (mutex-held m)))
     (cond ((not h) h)
	   ((eq? h (current-task))
	    (mutex-count m))
	   (else
	    h)))))

(define (mutex-waiters m)
  (without-interrupts
   (length (mutex-blocked m))))
