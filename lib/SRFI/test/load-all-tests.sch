(define (load-srfi-test n)
  (parameterize ((current-directory 
                  (string-append (current-larceny-root)
                                 "/lib/SRFI/test/")))
    (begin (display `(testing srfi ,n))
           (newline))
    (load (string-append "srfi-" (number->string n) "-test.sch"))))

(for-each 
 load-srfi-test
 '(0
   1
   2
   5
   6
   7
   8
   9
   11
   13
   14
   16
   17
   ;; 19 ;; depends on C FFI, which doesn't work in Common Larceny; see below
   ;; 22 ;; this is not a normal srfi test...
   23
   25
   26
   27
   28
   29
   30
   31
   37
   38
   39
   42
   45
   48
   51
   54
   60
   61
   64
   63 ;; SRFI 64 depends on SRFI 25, which SRFI 63 replaces
   66
   67
   69
   71
   74
   86
   87
   95
   98
   19 ;; depends on C FFI, which doesn't work in Common Larceny
   43 ;; redefines vector-map etc, so it has to come near the end
   78
   ))
