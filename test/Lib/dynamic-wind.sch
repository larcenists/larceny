; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test cases for dynamic-wind.

; Straightforward test -- does it get nesting right?

(define (run-dynamic-wind-tests)
  (display "Dynamic-wind") (newline)
  (continuation-tests)

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

; Continuations: the number of values thrown, throws that stay within a
; dynamic extent (the continuation does not reroot), out of and into
; extents, from an after thunk (interrupts must stay enabled), and
; re-entry.

(define (continuation-tests)

  (define (values-of thunk)
    (call-with-values thunk list))

  (define (interrupts-enabled?)
    (let ((ticks (disable-interrupts)))
      (if ticks (enable-interrupts ticks))
      (and ticks #t)))

  (allof "Continuations"

   (test "throw 0 values"
         (values-of (lambda () (call/cc (lambda (k) (k)))))
         '())
   (test "throw 1 value"
         (values-of (lambda () (call/cc (lambda (k) (k 'a)))))
         '(a))
   (test "throw 2 values"
         (values-of (lambda () (call/cc (lambda (k) (k 'a 'b)))))
         '(a b))
   (test "throw 3 values"
         (values-of (lambda () (call/cc (lambda (k) (k 'a 'b 'c)))))
         '(a b c))
   (test "return without throwing"
         (values-of (lambda () (call/cc (lambda (k) (values 1 2)))))
         '(1 2))

   (test "throw within an extent runs no thunks"
         (let ((trace '()))
           (dynamic-wind
            (lambda () (set! trace (cons 'before trace)))
            (lambda ()
              (let ((x (call/cc (lambda (k) (+ 1 (k 41))))))
                (set! trace (cons x trace))))
            (lambda () (set! trace (cons 'after trace))))
           (reverse trace))
         '(before 41 after))

   (test "throw out of nested extents"
         (let ((trace '()))
           (call/cc
            (lambda (k)
              (dynamic-wind
               (lambda () (set! trace (cons 'in1 trace)))
               (lambda ()
                 (dynamic-wind
                  (lambda () (set! trace (cons 'in2 trace)))
                  (lambda () (k 'done))
                  (lambda () (set! trace (cons 'out2 trace)))))
               (lambda () (set! trace (cons 'out1 trace))))))
           (reverse trace))
         '(in1 in2 out2 out1))

   (test "throw back into an extent"
         (let ((trace '())
               (again #f)
               (n 0))
           (dynamic-wind
            (lambda () (set! trace (cons 'in trace)))
            (lambda () (call/cc (lambda (k) (set! again k))))
            (lambda () (set! trace (cons 'out trace))))
           (set! n (+ n 1))
           (if (< n 3) (again 'x))
           (list n (reverse trace)))
         '(3 (in out in out in out)))

   (test "throw from an after thunk keeps interrupts enabled"
         (begin
           (call/cc
            (lambda (k)
              (dynamic-wind
               (lambda () #t)
               (lambda () (k 1))
               (lambda ()
                 (call/cc (lambda (k2) (k2 2)))))))
           (call/cc
            (lambda (k)
              (dynamic-wind
               (lambda () #t)
               (lambda () #f)
               (lambda () (k 3)))))
           (interrupts-enabled?))
         #t)

   (test "re-entering a continuation"
         (let ((k #f) (n 0) (results '()))
           (let ((v (call/cc (lambda (c) (set! k c) 0))))
             (set! results (cons v results))
             (set! n (+ n 1))
             (if (< n 5) (k (* n 10))))
           (reverse results))
         '(0 10 20 30 40))))

; (The continuation marks of a continuation are tested in wcm.sch.)
