; Use-once synchronization barrier.
; 2004-01-18 / lth
;
; MAKE-BARRIER num-tasks [name]  =>  barrier
;   Create a barrier for num-tasks tasks.  Name is an optional
;   string naming the barrier.
;
; ENTER-BARRIER barrier
;   Enter the barrier to block until num-tasks tasks have entered.
;
; BARRIER-NAME barrier => string
;   Get the barrier's name

(require 'tasking)
(require 'define-record)

(define-record barrier (remaining name blocked))

(define make-barrier
  (let ((make-barrier make-barrier))
    (lambda (num-tasks . rest)
      (let ((name (if (null? rest) "(anonymous)" name)))
	(make-barrier num-tasks name '())))))

(define (enter-barrier b)
  (without-interrupts
   (barrier-remaining-set! b (- (barrier-remaining b) 1))
   (cond ((negative? (barrier-remaining b))
	  (error "Barrier " (barrier-name b) " entered too many times."))
	 ((zero? (barrier-remaining b))
	  (for-each unblock (barrier-blocked b)))
	 (else
	  (barrier-blocked-set! b (cons (current-task) (barrier-blocked b)))
	  (block (current-task))))))
