; Testsuite/Lib/ctak.sch
; Larceny test suite -- TAK with continuations
;
; $Id$
;
; Partially tests call-with-current-continuation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         ctak.sch
; Description:  The ctak benchmark
; Author:       Richard Gabriel
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:53:02 (Bob Shaw)
;               24-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (run-ctak-tests)
  (allof "ctak tests"
	 (test "(ctak 18 12 6)" (ctak 18 12 6) 7)
	 (test "(ctak 18 12 6)" (ctak 18 12 6) 7)
	 (test "(ctak 18 12 6)" (ctak 18 12 6) 7)))


; The original version of this benchmark used a continuation mechanism that
; is less powerful than call-with-current-continuation and also relied on
; dynamic binding, which is not provided in standard Scheme.  Since the
; intent of the benchmark seemed to be to test non-local exits, the dynamic
; binding has been replaced here by lexical binding.

; For Scheme the comment that follows should read:
;;; CTAK -- A version of the TAK procedure that uses continuations.

;;; CTAK -- A version of the TAK function that uses the CATCH/THROW facility.

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k)
     (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))			;xy
      (k z)
      (call-with-current-continuation
       (ctak-aux
	k
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- x 1)
		     y
		     z)))
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- y 1)
		     z
		     x)))
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- z 1)
		     x
		     y)))))))

; eof
