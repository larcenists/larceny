; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; This program loads and exercises the lowlevel FFI in various ways.
;
; Some of the tests have been chosen because they test corners of the
; calling conventions: splitting data across registers and stack,
; across even/odd register pairs, and so on.  See ffi-test-ff.c for
; more details.

(define *work-path* "/home/lth/net/larceny/")

(define *ffi-path* (string-append *work-path* "Ffi/"))
(define *test-path* (string-append *work-path* "Testsuite/Lib/"))
(define *ffi-test-path* (string-append *work-path* "Testsuite/FFI/"))

(newline)
(display "*** FFI PATH IS ")
(display *ffi-path*)
(display " ***")
(newline)

; Run this procedure!

(define (run-basic-ffi-tests)
  (run-callout-tests)
  (run-callback-tests))

(load (string-append *ffi-path* "ffi-load.sch"))
(load (string-append *test-path* "test.sch"))

(define architecture)
(define callout-abi)
(define callback-abi)

(call-with-values
 (lambda ()
   (load-ffi *ffi-path*))
 (lambda (arch callout callback)
   (set! architecture arch)
   (set! callout-abi callout)
   (set! callback-abi callback)))

(ffi/libraries (cons (string-append *ffi-test-path* "ffi-test-ff.so")
                     (ffi/libraries)))

(define (fp name param ret)

  (define (rename p)
    (cdr (assq p '((int . signed32)
		   (short . signed32)
		   (char . signed32)
		   (unsigned . unsigned32)
		   (ushort . unsigned32)
		   (uchar . unsigned32)
		   (float . ieee32)
		   (double . ieee64)
		   (void . void)
		   (pointer . pointer)))))
			  
  (ffi/foreign-procedure callout-abi name 
			 (map rename param)
			 (rename ret)))

(define ffitest1 (fp "ffitest1" '() 'void))
(define ffitest2 (fp "ffitest2" '(int) 'int))
(define ffitest3 (fp "ffitest3" '(int int) 'int))
(define ffitest4 (fp "ffitest4" '(int int int int int int) 'double))
(define ffitest5 (fp "ffitest5" '(int int int int int int int int) 'float))
(define ffitest6 (fp "ffitest6" '(double) 'double))
(define ffitest7 (fp "ffitest7" '(double double) 'double))
(define ffitest8 (fp "ffitest8" '(double int double) 'float))
(define ffitest9 (fp "ffitest9" '(double double int double int) 'double))
(define ffitest10 (fp "ffitest10" '(float) 'float))
(define ffitest11 (fp "ffitest11" '(float float) 'float))

(define (run-callout-tests)
  (allof "FFI callout tests"
    (test "Test 1" (ffitest1) (unspecified))
    (test "Test 2" (ffitest2 1) 1)
    (test "Test 3" (ffitest3 1 2) 3)
    (test "Test 4" (ffitest4 1 2 3 4 5 6) 21.0)
    (test "Test 5" (ffitest5 1 2 3 4 5 6 7 8) 36.0)
    (test "Test 6" (ffitest6 1.0) 1.0)
    (test "Test 7" (ffitest7 1.0 2.0) 3.0)
    (test "Test 8" (ffitest8 1.0 2 3.0) 3.0)
    (test "Test 9" (ffitest9 1.0 2.0 3 4.0 5) 7.0)
    (test "Test 10" (ffitest10 1.0) 1.0)
    (test "Test 11" (ffitest11 1.0 2.0) 3.0)))

(define fficb1 (fp "fficb1" '(pointer) 'void))
(define fficb2 (fp "fficb2" '(pointer) 'int))
(define fficb3 (fp "fficb3" '(int pointer) 'int))

(define (make-callback proc args ret)
  (tr-code (ffi/make-callback callback-abi proc args ret)))

(define *the-value* #f)

(define cb:void->void
  (make-callback (lambda ()
                   (set! *the-value* 'cb:void->void))
		 '()
		 'void))

(define cb:void->int
  (make-callback (lambda ()
                   (set! *the-value* 'cb:void->int)
		   37)
		 '()
		 'signed32))

(define cb:int->int
  (make-callback (lambda (x)
                   (set! *the-value* 'cb:int->int)
		   (* x 2))
		 '(signed32)
		 'signed32))

(define (run-callback-tests)
  (allof "Callback tests"
    (test "Callback 1" 
          (let ((r (fficb1 cb:void->void)))
            (cons r *the-value*))
          '(#!unspecified . cb:void->void))
    (test "Callback 2"
          (let ((r (fficb2 cb:void->int)))
            (cons r *the-value*))
          '(37 . cb:void->int))
    (test "Callback 3"
          (let ((r (fficb3 37 cb:int->int)))
            (cons r *the-value*))
          '(74 . cb:int->int))))

; eof
