; AUTOMATED TESTING SCAFFOLDING.

; Generic test procedure; compares answer to expected answer.
; It requires that the answer and the expected answer are represented in
; a way which exercises only trusted procedures.

(define (test id ans correct)
  (if (not (equal? ans correct))
      (begin (display id) (display " did not pass test.") (newline)
	     (display "answer=") (display ans) (newline)
	     (display "correct=") (display correct) (newline)
	     #f)
      #t))

(define (allof . x)
  (let loop ((l x))
    (cond ((null? l) #t)
	  ((not (car l)) #f)
	  (else (loop (cdr l))))))


; eof
