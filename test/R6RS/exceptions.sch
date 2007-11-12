; This R6RS top-level program tests the R6RS exception system.

(import (rnrs base)
        (rnrs exceptions)
        (rnrs conditions)
        (rnrs io simple))

(call-with-current-continuation
 (lambda (k)
   (guard
    (x
     (#t
      (write (if (condition? x) (simple-conditions x) x))
      (newline)
      (newline)
      (k 0)
      (raise x)))
    (+ 1 2 3 'four 5 6))))

(write
 (guard (con
         ((error? con)
          (if (message-condition? con)
              (display (condition-message con))
              (display "an error has occurred"))
          (newline)
          'error)
         ((violation? con)
          (if (message-condition? con)
              (display (condition-message con))
              (display "the program has a bug"))
          (newline)
          'violation))
   (raise
    (condition
     (make-error)
     (make-message-condition "I am error:")))))

(newline)

(write
 (with-exception-handler
   (lambda (con)
     (cond ((not (warning? con))
            (raise con))
           ((message-condition? con)
            (display (condition-message con))
            (newline))
           (else
            (display "a warning has been issued")
            (newline)))
     42)
   (lambda ()
     (+ (raise-continuable
         (condition
          (make-warning)
          (make-message-condition
           "should be a number")))
        23))))

(newline)

'
(guard (con
        ((error? con)
         (if (message-condition? con)
             (display (condition-message con))
             (display "an error has occurred"))
         'error))
 (raise
  (condition (make-violation)
             (make-message-condition "I am an error"))))

(call-with-current-continuation
 (lambda (k)
   (guard
    (x
     (#t
      (write (if (condition? x) (simple-conditions x) x))
      (newline)
      (newline)
     ;(k 0)
      (raise x)))
    (+ 1 2 3 'four 5 6))))

