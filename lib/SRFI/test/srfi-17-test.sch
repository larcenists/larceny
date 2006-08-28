; Test suite for SRFI-17
; 2004-01-01 / lth

(cond-expand (srfi-17))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? '(37 . 2) (let ((v (cons 1 2)))
			(set! (car v) 37)
			v))
    (fail 'car))

(or (equal? '(1 . 37) (let ((v (cons 1 2)))
			(set! (cdr v) 37)
			v))
    (fail 'cdr))

(or (equal? '((37 2) 3) (let ((v (list (list 1 2) 3)))
			(set! (caar v) 37)
			v))
    (fail 'caar))

(or (equal? '(1 37 3) (let ((v (list 1 2 3)))
			(set! (cadr v) 37)
			v))
    (fail 'cadr))

(or (equal? '((1 . 37) 3) (let ((v (list (list 1 2) 3)))
			(set! (cdar v) 37)
			v))
    (fail 'cdar))

(or (equal? '(1 2 . 37) (let ((v (list 1 2 3)))
			  (set! (cddr v) 37)
			  v))
    (fail 'cddr))

; Oh, I grow weak...

(or (equal? '#(1 2 37) (let ((v (vector 1 2 3)))
			 (set! (vector-ref v 2) 37)
			 v))
    (fail 'vector-ref))

(or (equal? "ab%" (let ((v "abc"))
		    (set! (string-ref v 2) #\%) ; 37
		    v))
    (fail 'string-ref))

; Test the ability to define our own setters.

(define (make-glarg x) (vector x))
(define (glarg-ref x) (vector-ref x 0))
(define (glarg-set! x y) (vector-set! x 0 y))

(set! (setter glarg-ref) glarg-set!)

(or (equal? 37 (let ((v (make-glarg 0)))
		 (set! (glarg-ref v) 37)
		 (glarg-ref v)))
    (fail 'glarg-ref))

; getter-with-setters

(define (make-blarg x) (vector x))
(define blarg-ref (getter-with-setter (lambda (x) (vector-ref x 0)) 
				      (lambda (x y) (vector-set! x 0 y))))

(or (equal? 37 (let ((v (make-blarg 0)))
		 (set! (blarg-ref v) 37)
		 (blarg-ref v)))
    (fail 'blarg-ref))

(writeln "Done.")
