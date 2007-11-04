; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test cases for dynamic-wind.

; Straightforward test -- does it get nesting right?

(define (run-dynamic-wind-tests)
  (display "Dynamic-wind") (newline)

  (allof "Dynamic-wind"

   (test "dynamic-wind test"
	 (test-dynamic-wind)
	 dynamic-wind-test-result)

   ; See http://www.r6rs.org/r6rs-editors/2006-June/001319.html

   (test "dynamic-wind example 1"
         (let ((n 0))
           (call-with-current-continuation
            (lambda (k)
              (dynamic-wind
               (lambda ()
                ;(display "Executing in thunk") (newline)
                 (set! n (+ n 1))
                 (k #f))
               (lambda () #f)
               (lambda ()
                ;(display "Executing out thunk") (newline)
                 (set! n (+ n 1))))))
           n)
         1)

   (test "dynamic-wind example 2"
         (let ((n 0))
           (call-with-current-continuation
            (lambda (k)
              (dynamic-wind
               (lambda ()
                ;(display "Executing in thunk") (newline)
                 #t)
               (lambda () #f)
               (lambda ()
                ;(display "Executing out thunk") (newline)
                 (set! n (+ n 1))
                 (k #f)))))
           n)
         1)
   ))

(define (test-dynamic-wind)
  (define trace '())
  (define escape (lambda (n) (error "Escape.")))
  (define return (lambda (x) (loop x)))

  (define (loop n)
    (define (before)
      (set! trace (cons (cons 'before n) trace)))

    (define (during)
      (cond ((zero? (remainder n 4))
	     (call-with-current-continuation
	      (lambda (here)
		(set! return here)
		(escape n))))
	    ((zero? (remainder n 9))
	     (set! return (lambda (x) x))
	     (let ((v (call-with-current-continuation
		       (lambda (here)
			 (set! escape here)))))
	       (return v))))
      (loop (- n 1)))

    (define (after)
      (set! trace (cons (cons 'after n) trace)))

    (if (= n 0)
	(set! trace (cons 'bottom trace))
	(dynamic-wind before during after)))
    
  (let ((v (call-with-current-continuation
	    (lambda (k)
	      (set! escape k)
	      20)))) 
    (return v))
  (reverse trace))

(define dynamic-wind-test-result
  '((before . 20)
    (after . 20)
    (before . 20)
    (before . 19)
    (before . 18)
    (before . 17)
    (before . 16)
    (after . 16)
    (after . 17)
    (before . 17)
    (before . 16)
    (before . 15)
    (before . 14)
    (before . 13)
    (before . 12)
    (after . 12)
    (after . 13)
    (after . 14)
    (after . 15)
    (after . 16)
    (after . 17)
    (before . 17)
    (before . 16)
    (before . 15)
    (before . 14)
    (before . 13)
    (before . 12)
    (before . 11)
    (before . 10)
    (before . 9)
    (before . 8)
    (after . 8)
    (before . 8)
    (before . 7)
    (before . 6)
    (before . 5)
    (before . 4)
    (after . 4)
    (after . 5)
    (after . 6)
    (after . 7)
    (after . 8)
    (before . 8)
    (before . 7)
    (before . 6)
    (before . 5)
    (before . 4)
    (before . 3)
    (before . 2)
    (before . 1)
    bottom
    (after . 1)
    (after . 2)
    (after . 3)
    (after . 4)
    (after . 5)
    (after . 6)
    (after . 7)
    (after . 8)
    (after . 9)
    (after . 10)
    (after . 11)
    (after . 12)
    (after . 13)
    (after . 14)
    (after . 15)
    (after . 16)
    (after . 17)
    (after . 18)
    (after . 19)
    (after . 20)))

; eof
