; Tests of how the FFI passes arguments and results: in registers and
; on the stack, integers and floating-point numbers mixed, 64-bit
; values, sign extension of results, and pointers into bytevectors.
; The expected values are those computed by C.
;
; To run: build the foreign file (make macosx, or make linux), then in
; this directory
;
;     larceny -- abi-test.sch

(require 'std-ffi)

(ffi/libraries (cons (string-append (current-directory) "/"
                                    (if (string=? "MacOS X"
                                                  (cdr (assq 'os-name
                                                             (system-features))))
                                        "abi-test-ff.dylib"
                                        "abi-test-ff.so"))
                     (ffi/libraries)))

(define (fp name args ret) (foreign-procedure name args ret))

(define t-ints12
  (fp "t_ints12" '(int int int int int int int int int int int int) 'int))
(define t-mixed
  (fp "t_mixed" '(int double longlong float int double) 'longlong))
(define t-dbl10
  (fp "t_dbl10"
      '(double double double double double double double double double double)
      'double))
(define t-flt10
  (fp "t_flt10"
      '(float float float float float float float float float float)
      'float))
(define t-stackmix
  (fp "t_stackmix"
      '(int int int int int int int int
        double double double double double double double double
        int longlong int float double)
      'double))
(define t-neg (fp "t_neg" '() 'int))
(define t-ubig (fp "t_ubig" '() 'uint))
(define t-llmin (fp "t_llmin" '() 'longlong))
(define t-ullmax (fp "t_ullmax" '() 'ulonglong))
(define t-ullid (fp "t_ullid" '(ulonglong) 'ulonglong))
(define t-llid (fp "t_llid" '(longlong) 'longlong))
(define t-fill (fp "t_fill" '(boxed int) 'void))
(define t-sum-bytes (fp "t_sum_bytes" '(boxed int) 'int))

(define failures 0)

(define (check name actual expected)
  (if (not (equal? actual expected))
      (begin (set! failures (+ failures 1))
             (display "FAILED: ") (display name)
             (display ": got ") (write actual)
             (display ", expected ") (write expected)
             (newline))))

(check "12 ints" (t-ints12 1 -2 3 -4 5 -6 7 -8 9 -10 11 -12) -78)
(check "mixed" (t-mixed -5 1.5 10000000000 2.25 7 -3.5) 10000001692)
(check "10 doubles" (t-dbl10 1.5 2. 3. 4. 5. 6. 7. 8. 9.25 10.5) 392.75)
(check "10 floats" (t-flt10 1.5 2. 3. 4. 5. 6. 7. 8. 9.25 10.5) 392.75)
(check "stack mix"
       (t-stackmix 1 2 3 4 5 6 7 8 .5 .5 .5 .5 .5 .5 .5 .5
                   -3 40000000000 5 .25 1.5)
       400000015787.)
(check "int result" (t-neg) -7)
(check "uint result" (t-ubig) 4294967280)
(check "longlong result" (t-llmin) -9223372036854775808)
(check "ulonglong result" (t-ullmax) 18446744073709551615)
(check "ulonglong arg" (t-ullid 18446744073709551000) 18446744073709551000)
(check "longlong arg" (t-llid -9223372036854775000) -9223372036854775000)
(check "bytevector arg"
       (let ((b (make-bytevector 10 0))) (t-fill b 10) (t-sum-bytes b 10))
       135)

(display (if (= failures 0) "abi-test: ok" "abi-test: FAILED"))
(newline)
(exit (if (= failures 0) 0 1))
