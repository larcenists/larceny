; Test cases for SRFI-34
; 2004-01-11 / lth
;
; Culled from the examples section of SRFI-34.

; NOT COMPLETE

(cond-expand (srfi-34))

(call-with-current-continuation
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "condition: ")
                             (write x)
                             (newline)
                             (k 'exception))
     (lambda ()
       (+ 1 (raise 'an-error))))))
PRINTS: condition: an-error
=> exception

(call-with-current-continuation
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "something went wrong")
                             (newline)
                             'dont-care)
     (lambda ()
       (+ 1 (raise 'an-error))))))
PRINTS: something went wrong
then behaves in an unspecified way

(guard (condition
         (else
          (display "condition: ")
          (write condition)
          (newline)
          'exception))
  (+ 1 (raise 'an-error)))
PRINTS: condition: an-error
=> exception

(guard (condition
         (else
          (display "something went wrong")
          (newline)
          'dont-care))
 (+ 1 (raise 'an-error)))
PRINTS: something went wrong
=> dont-care

(call-with-current-continuation
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "reraised ") (write x) (newline)
                             (k 'zero))
     (lambda ()
       (guard (condition
                ((positive? condition) 'positive)
                ((negative? condition) 'negative))
        (raise 1))))))
=> positive

(call-with-current-continuation
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "reraised ") (write x) (newline)
                             (k 'zero))
     (lambda ()
       (guard (condition
                ((positive? condition) 'positive)
                ((negative? condition) 'negative))
        (raise -1))))))
=> negative

(call-with-current-continuation
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "reraised ") (write x) (newline)
                             (k 'zero))
     (lambda ()
       (guard (condition
                ((positive? condition) 'positive)
                ((negative? condition) 'negative))
        (raise 0))))))
PRINTS: reraised 0
=> zero

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'a 42))))
=> 42

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'b 23))))
=> (b . 23)