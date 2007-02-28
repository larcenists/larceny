
(define-syntax wcm
  (syntax-rules ()
    ((wcm key mark expr)
     (call-with-continuation-mark key mark (lambda () expr)))))

(define-syntax if-mf
  (syntax-rules ()
    ((if-mf then else)
     (call-if-continuation-mark-frame (lambda () then)
                                      (lambda () else)))))
(define-syntax if-rep
  (syntax-rules ()
    ((if-rep key then else)
     (call-if-continuation-mark-replace key
                                        (lambda () then)
                                        (lambda () else)))))

(define (ccm key)
  (continuation-mark-set->list (current-continuation-marks) key))

(define (ccm* keys)
  (continuation-mark-set->list* (current-continuation-marks) keys))

(define (run-wcm-tests)
  (display "Continuation Marks") (newline)
  (allof "with-continuation-mark, current-continuation-marks"
    (test "wcm 1 (new mark)"
          (wcm 'a 1 (ccm 'a))
          '(1))
    (test "wcm 2 (overwrite)"
          (wcm 'a 1 (wcm 'a 2 (ccm 'a)))
          '(2))
    (test "wcm 3 (multiple frames)"
          (wcm 'a 1 (wcm 'a 2 (values (wcm 'a 3 (ccm 'a)))))
          '(3 2))
    (test "wcm 4 (multiple marks)"
          (wcm 'a 1 (wcm 'b 2 (wcm 'a 3 (wcm 'b 4 (list (ccm 'a) (ccm 'b))))))
          '( (3) (4) ))
    (test "wcm 5 (dynamic-wind)"
          (let ((result1 #f)
                (result2 #f))
            (call-with-current-continuation
             (lambda (escape)
               (wcm 'a 1
                 (wcm 'a 2
                   (values
                    (wcm 'a 3
                      (dynamic-wind
                          (lambda () (set! result1 (ccm 'a)))
                          (lambda ()
                            (wcm 'a 'Sir-Not-Appearing-in-this-Continuation
                              (values
                               (wcm 'a 'nee!
                                 (escape #t)))))
                          (lambda () (set! result2 (ccm 'a))))))))))
            (list result1 result2))
          ;; If dynamic-wind were not changed to reset the continuation
          ;; mark stack on execution of the after-thunk, we'd get additional
          ;; marks in results2.
          '( (3 2) (3 2)))
    )

  (allof "continuation-marks "
    (test "cm 1 (mark from continuation)"
      (let ((kont #f))
        (wcm 'a 1
          (call-with-current-continuation
            (lambda (k)
              (set! kont k))))
        (continuation-mark-set->list
          (continuation-marks kont) 'a))
      '(1))
    (test "cm 2 (not escaping funny)"
      (let ((kont #f))
        (wcm 'a 1
          (call-with-current-continuation
            (lambda (k)
              (set! kont k))))
        (continuation-mark-set->list
          (current-continuation-marks) 'a))
      '())
    ) 

  (allof "continuation-mark-set->list*"
    (test "wcm* 1 (new mark)"
          (wcm 'a 1 (ccm* '(a)))
          '( #(1) ))
    (test "wcm* 2 (overwrite)"
          (wcm 'a 1 (wcm 'a 2 (ccm* '(a))))
          '( #(2) ))
    (test "wcm* 3 (multiple frames)"
          (wcm 'a 1 (wcm 'a 2 (values (wcm 'a 3 (ccm* '(a))))))
          '( #(3) #(2) ))
    (test "wcm* 4 (multiple marks)"
         (wcm 'a 1 (wcm 'b 2 (wcm 'a 3 (wcm 'b 4 (ccm* '(a b))))))
         '( #(3 4) ))
    (test "wcm* 5 (dynamic-wind)"
          (let ((result1 #f)
                (result2 #f))
            (call-with-current-continuation
             (lambda (escape)
               (wcm 'a 1
                 (wcm 'a 2
                   (values
                    (wcm 'a 3
                      (dynamic-wind
                          (lambda () (set! result1 (ccm* '(a))))
                          (lambda ()
                            (wcm 'a 'Sir-Not-Appearing-in-this-Continuation
                              (values
                               (wcm 'a 'nee!
                                 (escape #t)))))
                          (lambda () (set! result2 (ccm* '(a)))))))))))
            (list result1 result2))
          ;; If dynamic-wind were not changed to reset the continuation
          ;; mark stack on execution of the after-thunk, we'd get additional
          ;; marks in results2.
          '( (#(3) #(2)) (#(3) #(2))))
    (test "wcm* 6 (multiple with empties)"
         (wcm 'a 1
           (values
             (wcm 'b 2
               (wcm 'a 3
                 (ccm* '(a b))))))
         '(#(3 2) #(1 #f)))

    (test "wcm* 6 (multiple)"
         (wcm 'a 1
           (values
             (wcm 'c 4
               (values
                 (wcm 'b 2
                   (wcm 'a 3
                     (ccm* '(a b))))))))
         '(#(3 2) #(1 #f)))
    )

  (allof "call-if-continuation-mark-frame"
    (test "if-mf 1 (simple no)"
          (values
            (if-mf 1 2))
          2)
    (test "if-mf 2 (simple yes)"
          (values
            (wcm 'a 'b
              (if-mf 1 2)))
          1)
    (test "if-mf 3 (shielded no)"
          (values
            (wcm 'a 'b
              (values
                (if-mf 1 2))))
          2)
    (test "if-mf 4 (doesn't mask)"
          (values
            (wcm 'a 'b
              (if-mf
                (wcm 'a 'c
                  (ccm 'a))
                'wrong)))
          '(c))
    (test "if-mf 5 (doesn't unmask)"
          (values
            (wcm 'a 'b
              (values
                (if-mf
                  'wrong
                  (wcm 'a 'c
                    (ccm 'a))))))
          '(c b))
    (test "if-mf 6 (repeated, yes)"
          (values
            (wcm 'a 'b
              (if-mf
                (if-mf
                  1
                  2)
                3)))
          1)
    (test "if-mf 7 (repeated, no)"
          (values
            (wcm 'a 'b
              (values
                (if-mf
                  1
                  (if-mf
                    2
                    3)))))
          3)
    )

  (allof "call-if-continuation-mark-replace"
    (test "if-rep 1 (simple no)"
          (values
            (if-rep 'a 1 2))
          2)
    (test "if-rep 2 (simple yes)"
          (values
            (wcm 'a 'b
              (if-rep 'a 1 2)))
          1)
    (test "if-rep 3 (shielded no)"
          (values
            (wcm 'a 'b
              (values
                (if-rep 'a 1 2))))
          2)
    (test "if-rep 4 (doesn't mask)"
          (values
            (wcm 'a 'b
              (if-rep 'a
                (wcm 'a 'c
                  (ccm 'a))
                'wrong)))
          '(c))
    (test "if-rep 5 (doesn't unmask)"
          (values
            (wcm 'a 'b
              (values
                (if-rep 'a
                  'wrong
                  (wcm 'a 'c
                    (ccm 'a))))))
          '(c b))
    (test "if-rep 6 (repeated, yes)"
          (values
            (wcm 'a 'b
              (if-rep 'a
                (if-rep 'a
                  1
                  2)
                3)))
          1)
    (test "if-rep 7 (repeated, no)"
          (values
            (wcm 'a 'b
              (values
                (if-rep 'a
                  1
                  (if-rep 'a
                    2
                    3)))))
          3)
    (test "if-rep 8 (distinguishes)"
          (values
            (wcm 'a 'b
              (values
                (wcm 'c 'd
                  (if-rep 'a
                    1
                    2)))))
          2)
    (test "if-rep 9 (doesn't misdistinguish)"
          (values
            (wcm 'a 'b
              (wcm 'c 'd
                (if-rep 'a
                  1
                  2))))
          1)
    )
  )
