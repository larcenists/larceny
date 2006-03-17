; Test suite for SRFI-0
; 2003-12-29 / lth

(cond-expand (srfi-0))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define (inspect name)
  (car (environment-get-cell (interaction-environment) name)))

; Useful to use define + environment inspection here to ensure that
; the contents of cond-expand are in fact evaluated at the top level.

(cond-expand 
 ((not bigloo)
  (define defined-if-not-bigloo 37))
 (else))

(or (equal? 37 (inspect 'defined-if-not-bigloo))
    (fail 'feature-not-bigloo))

(cond-expand 
 (larceny
  (define defined-if-larceny 37))
 (else))

(or (equal? 37 (inspect 'defined-if-larceny))
    (fail 'feature-larceny))

(cond-expand
 (srfi-0
  (define defined-if-srfi-0 37))
 (else))

(or (equal? 37 (inspect 'defined-if-srfi-0))
    (fail 'feature-srfi-0))

(cond-expand
 (bigloo
  #t)
 (else
  (define defined-by-else 37)))

(or (equal? 37 (inspect 'defined-by-else))
    (fail 'cond-expand-else:1))

; Test that a syntax error is reported as it should be
; if the COND-EXPAND cannot be fulfilled.

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler (lambda args (k #t))))
	 (macro-expand '(cond-expand (bigloo #t)))
	 #f)))
    (fail 'cond-expand-noelse:1))

(writeln "Done.")
