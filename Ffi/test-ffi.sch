; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Simple FFI test code.

(load "Ffi/ffi-load.sch")

(define architecture)
(define callout-abi)
(define callback-abi)

(call-with-values
 (lambda ()
   (load-ffi "Ffi/"))
 (lambda (arch callout callback)
   (set! architecture arch)
   (set! callout-abi callout)
   (set! callback-abi callback)))

(case architecture
  ((sun4-sunos4)
   (ffi/libraries (cons "/proj/will/lth/larceny/Ffi/test-ffi.so"
			(ffi/libraries))))
  ((sun4-sunos5)
   (ffi/libraries (cons "/proj/will/lth/larceny/Ffi/test-ffi.o"
			(ffi/libraries))))
  (else ???))

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

(define (run-ffi-tests)
  (display (ffitest1)) (newline)
  (display (ffitest2 1)) (newline)
  (display (ffitest3 1 2)) (newline)
  (display (ffitest4 1 2 3 4 5 6)) (newline)
  (display (ffitest5 1 2 3 4 5 6 7 8)) (newline)
  (display (ffitest6 1.0)) (newline)
  (display (ffitest7 1.0 2.0)) (newline)
  (display (ffitest8 1.0 2 3.0)) (newline)
  (display (ffitest9 1.0 2.0 3 4.0 5)) (newline)
  (display (ffitest10 1.0)) (newline)
  (display (ffitest11 1.0 2.0)) (newline)
  (unspecified))

(define fficb1 (fp "fficb1" '(pointer) 'void))
(define fficb2 (fp "fficb2" '(pointer) 'int))
(define fficb3 (fp "fficb3" '(int pointer) 'int))

(define (make-callback proc args ret)
  (tr-code (ffi/make-callback callback-abi proc args ret)))

(define cb:void->void
  (make-callback (lambda ()
		   (display "callback void->void")
		   (newline))
		 '()
		 'void))

(define cb:void->int
  (make-callback (lambda ()
		   (display "callback void->int")
		   (newline)
		   37)
		 '()
		 'signed32))

(define cb:int->int
  (make-callback (lambda (x)
		   (display "callback int->int ")
		   (display x)
		   (newline)
		   (* x 2))
		 '(signed32)
		 'signed32))

(define (run-ffi-cbtests)
  (display (fficb1 cb:void->void)) (newline)
  (display (fficb2 cb:void->int)) (newline)
  (display (fficb3 37 cb:int->int)) (newline)
  (unspecified))

; eof
