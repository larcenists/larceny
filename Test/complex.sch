(define (generate-results)

  (define onearg
    (list sqrt sin cos tan asin acos atan))

  (define values
    (list -inf.0 -1234567.8901234 -1.0 0.0 1.0 1234567.8901234 +inf.0))

  (display (map (lambda (f)
		  (map f values))
		onearg))
  (newline))
