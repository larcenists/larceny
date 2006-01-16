
(define-syntax wcm
  (syntax-rules ()
    ((wcm key mark expr)
     (call-with-continuation-mark key mark (lambda () expr)))))

(define (ccm key)
  (continuation-mark-set->list (current-continuation-marks) key))

(define (run-wcm-tests)
  (display "Continuation Marks") (newline)
  (allof "Continuation Marks"
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
	  '( (3) (4) ))))

