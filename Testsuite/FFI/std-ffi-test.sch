; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Test cases for the standard FFI.
;
; The test of the basic FFI tests whether the fundamental parameter types
; can be passed in various locations and so on, so this file tests whether
; the std-ffi type names work, whether foreign-procedure and foreign-file
; and the null pointer functions work, and  so on.

(define *testsuite-dir* "/home/lth/net/larceny/Testsuite/")

(load (string-append *testsuite-dir* "Lib/test.sch"))

(define (run-std-ffi-tests)
  (std-ffi-test-null)
  (std-ffi-test-int)
  (std-ffi-test-uint)
  (std-ffi-test-char)
  (std-ffi-test-bool)
  (std-ffi-test-float)
  (std-ffi-test-string)
  (std-ffi-test-void)
  (std-ffi-test-get/set)
  (std-ffi-test-peek/poke))

(foreign-file (string-append *testsuite-dir* "FFI/std-ffi-test-ff.so"))

(define valid-null-pointer?
  (foreign-procedure "valid_null_pointer_p" '(unsigned) 'bool))

(define return-null-pointer
  (foreign-procedure "return_null_pointer" '() 'unsigned))

(define (std-ffi-test-null)
  (allof "FFI NULL cases"
   (test "sane?"
         (foreign-null-pointer? (foreign-null-pointer))
         #t)
   (test "generate correct NULL?"
         (valid-null-pointer? (foreign-null-pointer))
         #t)
   (test "NULL conversion twice"
         (valid-null-pointer? (return-null-pointer))
         #t)
   (test "Recognize the real NULL?"
         (foreign-null-pointer? (return-null-pointer))
         #t)
   ))

; Integer types

(define add-ints (foreign-procedure "add_ints" '(int int) 'int))
(define add-shorts (foreign-procedure "add_shorts" '(short short) 'short))
(define add-longs (foreign-procedure "add_longs" '(long long) 'long))
(define add-uints (foreign-procedure "add_uints" '(unsigned unsigned) 
                                     'unsigned))
(define add-ushorts (foreign-procedure "add_ushorts" '(ushort ushort) 'ushort))
(define add-ulongs (foreign-procedure "add_ulongs" '(ulong ulong) 'ulong))

(define (int-test x)
  (let ((name (car x))
        (p (cadr x)))
    (allof "int-test"
      (test (string-append "int-test " name)
            (p 12 24)
            36)
      (test (string-append "int-test " name)
            (p -1 (p (p 5 7) -11))
            0))))

(define (uint-test x)
  (let ((name (car x))
        (p (cadr x)))
    (allof "uint-test"
      (test (string-append "uint-test " name)
            (p 12 24)
            36))))

(define (std-ffi-test-int)
  (for-each int-test 
            `(("int" ,add-ints)
              ("short" ,add-shorts)
              ("long" ,add-longs))))

(define (std-ffi-test-uint)
  (for-each uint-test 
            `(("uint" ,add-uints)
              ("ushort" ,add-ushorts)
              ("ulong" ,add-ulongs))))

; Char types

(define compare-chars (foreign-procedure "cmp_chars" '(char char) 'char))
(define compare-uchars (foreign-procedure "cmp_chars" '(uchar uchar) 'uchar))

(define (std-ffi-test-char)
  (allof "Test char"
    (test "compare 1" (compare-chars #\A #\A) #\Y)
    (test "compare 2" (compare-chars #\A #\B) #\N)
    (test "compare 3" (compare-chars #\a #\a) #\y)
    (test "compare 4" (compare-chars #\a #\b) #\n)))

; Bool type

(define pass-bool->int
  (foreign-procedure "pass_bool2int" '(bool) 'int))

(define pass-bool->bool
  (foreign-procedure "pass_bool2bool" '(bool) 'bool))

(define (std-ffi-test-bool)
  (allof "bool tests"
    (test "pass #t as bool, return int (1 or 0)"
          (pass-bool->int #t)
          1)
    (test "pass #f as bool, return int (1 or 0)"
          (pass-bool->int #f)
          0)
    (test "pass pair as bool, return int (1 or 0)"
          (pass-bool->int (cons 1 2))
          1)
    (test "pass #t as bool, return bool"
          (pass-bool->bool #t)
          #t)
    (test "pass #f as bool, return bool"
          (pass-bool->bool #f)
          #f)
    (test "pass pair as bool, return bool"
          (pass-bool->bool (cons 1 2))
          #t)
    ))

; Floating types

(define add-doubles 
  (foreign-procedure "add_doubles" '(double double) 'double))

(define add-floats
  (foreign-procedure "add_floats" '(float float) 'float))

(define (floating-test x)
  (let ((name (car x))
        (p (cadr x)))

    (define (fixit x)
      (/ (round (* x 10000.0)) 10000.0))

    (allof "floating test"
      (test name (fixit (p 12.4 24.8)) 37.2)
      (test name (fixit (p -1.0 (p (p 5.4 7.0) -11.0))) 0.4))))

(define (std-ffi-test-float)
  (for-each floating-test 
            `(("double" ,add-doubles)
              ("float" ,add-floats))))
          
; String and boxed types

(define return-half                     ; Return second half of the string.
  (foreign-procedure "return_half" '(string) 'string))

(define fill-bytevector                 ; Fill by-reference bytevector.
  (foreign-procedure "fill_bytevector" '(boxed int) 'void))

(define pass-null-pointer               ; Pass and return #f as NULL.
  (foreign-procedure "pass_null_pointer" '(string) 'string))

(define pass-null-pointer-to-boxed      ; Pass #f as NULL, test result.
  (foreign-procedure "pass_null_pointer_to_boxed" '(boxed) 'bool))
                     
(define (std-ffi-test-string)
  (allof "string test"
    (test "pass and return null pointer through STRING"
          (pass-null-pointer #f)
          #f)
    (let ((x "abracadabra"))
      (test "pass and return STRING"
            (return-half (string-append x x))
            x))
    (test "pass bytevector through BOXED"
          (let ((bv (make-bytevector 10)))
            (bytevector-fill! bv 255)
            (fill-bytevector bv (bytevector-length bv))
            (do ((i 0 (+ i 1))
                 (res #t))
                ((= i (bytevector-length bv))
                 res)
              (if (not (= (bytevector-ref bv i) i))
                  (set! res #f))))
          #t)
    (test "passing #f through BOXED."
          (pass-null-pointer-to-boxed #f)
          #t)
    ))

; Void return type

(define void-return
  (foreign-procedure "void_return" '(boxed) 'void))

(define (std-ffi-test-void)
  (allof "void return"
    (let ((b (make-bytevector sizeof:int)))
      (bytevector-set! b 0 0)
      (test "void return"
            (begin (void-return b)
                   (bytevector-ref b 0))
            1))
    ))

; Get/set procedures

(define (std-ffi-test-get/set)
  ; FIXME
  #t)

; peek/poke procedures

(define (std-ffi-test-peek/poke)
  ; FIXME
  #t)

; eof 
