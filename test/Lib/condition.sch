; Condition tests.

(define (run-condition-tests)
  (display "Condition") (newline)
  (condition-test-0))

; Examples straight out of the R6RS.

(define (condition-test-0)
  (define-condition-type &c &condition
    make-c c?
    (x c-x))
  
  (define-condition-type &c1 &c
    make-c1 c1?
    (a c1-a))
  
  (define-condition-type &c2 &c
    make-c2 c2?
    (b c2-b))
  
  (define v1 (make-c1 "V1" "a1"))
  (define v2 (make-c2 "V2" "b2"))
  (define v3 (condition
              (make-c1 "V3/1" "a3")
              (make-c2 "V3/2" "b3")))
  (define v4 (condition v1 v2))
  (define v5 (condition v2 v3))

  (allof

   (test "(c? v1)" (c? v1) #t)
   (test "(c1? v1)" (c1? v1) #t)
   (test "(c2? v1)" (c2? v1) #f)
   (test "(c-x v1)" (c-x v1) "V1")
   (test "(c1-a v1)" (c1-a v1) "a1")

   (test "(c? v2)" (c? v2) #t)
   (test "(c1? v2)" (c1? v2) #f)
   (test "(c2? v2)" (c2? v2) #t)
   (test "(c-x v2)" (c-x v2) "V2")
   (test "(c2-b v2)" (c2-b v2) "b2")

   (test "(c? v3)" (c? v3) #t)
   (test "(c1? v3)" (c1? v3) #t)
   (test "(c2? v3)" (c2? v3) #t)
   (test "(c-x v3)" (c-x v3) "V3/1")
   (test "(c1-a v3)" (c1-a v3) "a3")
   (test "(c2-b v3)" (c2-b v3) "b3")

   (test "(c? v4)" (c? v4) #t)
   (test "(c1? v4)" (c1? v4) #t)
   (test "(c2? v4)" (c2? v4) #t)
   (test "(c-x v4)" (c-x v4) "V1")
   (test "(c1-a v4)" (c1-a v4) "a1")
   (test "(c2-b v4)" (c2-b v4) "b2")

   (test "(c? v5)" (c? v5) #t)
   (test "(c1? v5)" (c1? v5) #t)
   (test "(c2? v5)" (c2? v5) #t)
   (test "(c-x v5)" (c-x v5) "V2")
   (test "(c1-a v5)" (c1-a v5) "a3")
   (test "(c2-b v5)" (c2-b v5) "b2")
   ))

; FIXME: examples from R6RS library section 7.1.

(define (condition-test-1)
'
  (guard (con
          ((error? con)
           (if (message-condition? con)
               (display (condition-message con))
               (display "an error has occurred"))
           'error)
          ((violation? con)
           (if (message-condition? con)
               (display (condition-message con))
               (display "the program has a bug"))
           'violation))
    (raise
     (condition
      (make-error)
      (make-message-condition "I am an error"))))
'
  (guard (con
          ((error? con)
           (if (message-condition? con)
               (display (condition-message con))
               (display "an error has occurred"))
           'error))
   (raise
    (condition (make-violation)
               (make-message-condition "I am an error"))))
'
  (with-exception-handler
    (lambda (con)
      (cond ((not (warning? con))
             (raise con))
            ((message-condition? con)
             (display (condition-message con)))
            (else
             (display "a warning has been issued")))
      42)
    (lambda ()
      (+ (raise-continuable
          (condition
           (make-warning)
           (make-message-condition
            "should be a number")))
         23)))
  )
