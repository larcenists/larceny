; Test code for tasking
; 2004-01-18 / lth

(require 'tasking)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(define the-output '())

(define (output m)
  (set! the-output (append the-output (list m))))

; Some setup, tests are at the end

(define tasks 3)                      ; # of tasks to run

(define (tasking-test-1) 
  (set! the-output '())
  (with-tasking (lambda ()
		  (tasking-task-switch-test #t)))
  the-output)

(define (tasking-test-2)
  (set! the-output '())
  (with-tasking (lambda ()
		  (tasking-task-switch-test #f)))
  the-output)

(define (tasking-task-switch-test yield?)

  (define finished 0)                   ; # of tasks completed

  ; Spawn some worker tasks

  (do ((i 0 (+ i 1)))
      ((= i tasks))
    (spawn (lambda ()
	     (do ((j 0 (+ j 1)))
		 ((= j 10)
		  (without-interrupts
		   (set! finished (+ finished 1))
		   (output (list 'done i))))
	       (without-interrupts
		(output (list 'iter i j)))
	       (if yield?
		   (yield)
		   (do ((i 0 (+ i 1)))
		       ((= i 2000))))))))

  ; Wait for the tasks to finish

  (let loop ()
    (if (without-interrupts
         (if (not (= finished tasks))
             (begin (yield)
                    #t)
             #f))
        (loop))))

(define (task-check fn id)
  (let ((o (fn)))
    (do ((i 0 (+ i 1)))
	((= i tasks))
      (if (not (member (list 'done i) o))
	  (fail (string-append id ":done")))
      (do ((j 0 (+ j 1)))
	  ((= j 10))
	(if (not (member (list 'iter i j) o))
	    (fail (string-append id ":iter")))))))

(task-check tasking-test-1 "task1")
(task-check tasking-test-2 "task2")

(display "Done.")
(newline)

; eof
