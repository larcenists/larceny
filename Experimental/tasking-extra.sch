; Useful support code for tasking package.
; Not all debugged; use with care.

; To use the non-I/O-aware tasks in an interactive system you need to 
; make sure control is not returned to the interactive task while the
; computation is ongoing, because the interactive task will block the
; system.  The procedure call-without-current-task can be used to
; block the current task until 'thunk' completes; if used interactively,
; the current task will be the interactive task.

(define (call-without-current-task thunk)
  (let ((interactive (current-task))    ; Is this right?
        (r #f)
        (b (make-barrier 2)))
    (spawn (lambda ()
             (set! r (thunk))
             (enter-barrier b)))
    (enter-barrier b)
    r))


; This is a use-once synchronization barrier.  Call make-barrier with
; the number of tasks that will enter the barrier.  Call enter-barrier
; to block until the desired number of tasks have entered the barrier.

(define (make-barrier num-tasks)
  (let ((n 0)
        (blocked '()))
    (lambda ()
      (without-interrupts
       (set! n (+ n 1))
       (cond ((> n num-tasks)
              (error "Barrier entered more than " num-tasks " times."))
             ((= n num-tasks)
              (for-each unblock blocked))
             (else
              (set! blocked (cons (current-task) blocked))
              (block (current-task))))))))

(define (enter-barrier b) (b))


; Mutex.  A mutex can be acquired multiple times by the same thread.

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
     (cond ((not (eq? h (current-task)))
            (error "MUTEX-RELEASE: mutex " m " not held by " (current-task)))
           (else
            (let ((c (mutex-count m)))
              (mutex-count-set! m (- c 1))
              (if (= c 1)
                  (let ((b (mutex-blocked m)))
                    (mutex-blocked-set! m '())
                    (mutex-held-set! m #f)
                    (for-each unblock b)))))))))


; Synchronized LAMBDA (a monitor).

(define-syntax sync-lambda 
  (syntax-rules ()
    ((sync-lambda FORMALS BODY ...)
     (let ((m (make-mutex)))
       (lambda FORMALS
         (dynamic-wind 
          (lambda () (mutex-acquire m))
          (lambda ()
            BODY ...)
          (lambda () (mutex-release m))))))))


; Test code

(define (tasking-test-1) (tasking-task-switch-test #t))
(define (tasking-test-2) (tasking-task-switch-test #f))

(define (tasking-task-switch-test yield?)

  (define finished 0)                   ; tasks completed
  (define tasks 3)                      ; tasks to run

  (spawn-useless-loops tasks yield?)

  ; This loop lets the tasks complete before control is returned to the
  ; read-eval-print loop (which will inhibit the switching).

  (let loop ()
    (if (without-interrupts
         (if (not (= finished tasks))
             (begin (yield)
                    #t)
             #f))
        (loop))))

(define (spawn-useless-loops tasks yield?)
  (do ((i 0 (+ i 1)))
      ((= i tasks))
    (spawn (make-useless-loop i yield?))))

(define (make-useless-loop i yield?)
  (lambda ()
    (do ((j 0 (+ j 1)))
        ((= j 10)
         (without-interrupts
          (set! finished (+ finished 1))
          (format #t "Task ~a done.~%" i)))
      (without-interrupts
       (format #t "Task ~a iteration ~a.~%" i j))
      (if yield?
          (yield)
          (do ((i 0 (+ i 1)))
              ((= i 2000)))))))

(define (tasking-test-3)

  (define tasks 3)

  (let ((b (make-barrier (+ tasks 1))))
    (do ((i 0 (+ i 1)))
        ((= i tasks))
      (spawn (lambda ()
               ((make-useless-loop i #t))
               (enter-barrier b))))
    (enter-barrier b)))

(define (tasking-test-4)
  (call-without-interactive-task 
   (lambda ()
     (spawn-useless-loops 3 #f))))

; eof
