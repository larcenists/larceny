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
   12
   13
   14
   16
   17
   19
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
   60
   66
   69
   ))
