; Ffi/ffi-test.sch

(load "Ffi/loadffi.sch")
(load-ffi "Ffi/" 'sun4-sunos4)

; (define puts
;   (let ((puts (ffi/foreign-procedure ffi/SPARC-sunos4-C-callout-stdabi
; 				     "puts" '(pointer) 'signed32)))
;     (lambda (s)
;       (puts (ffi/string->asciiz s)))))
; 
; (define cbtest
;   (let ((cbtest (ffi/foreign-procedure ffi/SPARC-sunos4-C-callout-stdabi
; 				       "cbtest" '(pointer) 'void)))
;     (lambda ()
;       (cbtest (tr-code the-callback)))))
; 
; (define the-callback
;   (ffi/make-callback ffi/SPARC-sunos4-C-callback-stdabi
; 		     (lambda ()
; 		       (display "Hello, Scheme world!")
; 		       (newline))
; 		     '()
; 		     'void))

(ffi/libraries (cons "/proj/will/lth/larceny/Ffi/ffi-test.so"
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
			  
  (ffi/foreign-procedure ffi/SPARC-sunos4-C-callout-stdabi name 
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
  (tr-code (ffi/make-callback ffi/SPARC-sunos4-C-callback-stdabi
			      proc args ret)))

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
