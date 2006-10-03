; Various number-theoretic functions.
; 2002-01-12 / lth

; From: oleg@pobox.com (oleg@pobox.com)
; Subject: Re: arbritary precision rationals
; Newsgroups: comp.lang.scheme
; Date: 13 Nov 2001 15:07:34 -0800

; This article shows a not-so-naive realization of the Eratosthenes
; Sieve. The code computes a sequence of prime numbers not exceeding a
; given number N, rather fast. The sophistication of the algorithm is in
; avoiding wasting time and space on even numbers. This message also
; shows the example of computing with exact rationals.
;
; The Eratosthenes sieve algorithm is described in
; 	http://mathworld.wolfram.com/EratosthenesSieve.html
;
; In the naive implementation, we would use a vector v of size N+1. v[i]
; is a boolean indicating the status of integer i, i=0..N. v[i] is #f if
; integer i has been eliminated (dropped through the sieve). Initially v
; is set to #t, with the exception v[0]=v[1]=#f (obviously 0 and 1 are
; not primes). We start by locating the first i (greater than the last
; found prime) so that v[i] is #t. We add v[i] to the list of found
; primes, and eliminate all multiples of i. That is, we set v[2i],
; v[3i], etc. to #f.  We repeat the process until there is nothing to
; eliminate.
;	
; Since all primes except 2 are odd, it is wasteful to even consider even
; numbers. Now, we will interpret v[i] as an indication of the status of
; a number n(i) = (2*i)+3. Initially, all elements of the vector are set
; to #t. We locate the first i in the yet-to-be-examined portion of v 
; so that v[i] is #t. Such i corresponds to a number p(i)= (2*i)+3. We
; add this number to the list of found primes and eliminate all its
; multiples. Because we do not consider even numbers, we need to
; eliminate only odd multiples, (2k+1)*(2*i+3). k=1.... That is, we need to set
; 	v[ ( (2k+1)*(2*i+3) - 3 ) / 2 ] to #f
; or
; 	v[ k*p(i) + i] = #f
;	
; The algorithm becomes
	
(define (prime-sieve N)
  (let* ((max-index (quotient (- N 3) 2))
	 (v         (make-vector (+ 1 max-index) #t)))
    ;; i is the current index on the tape
    ;; primes is the list of found primes, in reverse order
    (let loop ((i 0) (primes '(2)))
      (cond ((> i max-index) 
	     (reverse primes))
	    ((vector-ref v i)
	     (let ((prime (+ i i 3)))	; newly found prime
	       (do ((j (+ 3 (* 3 i)) (+ j prime)))
		   ((> j max-index))
		 (vector-set! v j #f))
	       (loop (+ 1 i) (cons prime primes))))
	    (else
	     (loop (+ 1 i) primes))))))

; Naive, and recomputes the primes every time.
; The square root may cause precision to be lost if it always
; casts to inexact (as does Larceny).

(define (factor-number n)
  (cond ((or (< n 1) (not (integer? n)))
	 (error "Not factorable: " n))
	(else
	 (let* ((limit  (inexact->exact (ceiling (sqrt n))))
		(primes (prime-sieve limit)))
	   (let loop ((n n) (p primes) (f '()))
	     (cond ((null? p)
		    (if (or (not (= n 1)) (null? f))
			(cons n f)
			f))
		   ((zero? (remainder n (car p)))
		    (loop (quotient n (car p))
			  primes
			  (cons (car p) f)))
		   (else
		    (loop n (cdr p) f))))))))

; Produces exact output for exact input

; given x find y = f(x) s.t. y*y = x

; x - sqrt(x)*sqrt(x) = 0

(define (square-root n)
  
  ; sqrt'(x) = 1/(2 sqrt(x))
  ;
  ; f(x)  = n - sqrt(x)*sqrt(x)
  ; f'(x) = 0 - 2 sqrt(x)
  ;       = 2(sqrt(x)*sqrt(x)') - 0
  ;       = 2(x^0.5 * 0.5 * x^-0.5) - 0
  ;       = sqrt(x) / sqrt(x) - 0
  ;       = 1

  (define (newton i p0)
    (if (> i 10) 
	(undefined)
	(let ((p (- p0 (/ (- (* p0 p0) n) (* 2 p0)))))
	  (display p)
	  (display "   ")
;	  (display (exact->inexact p))
	  (newline)
	  (newton (+ i 1) p))))

  (newton 0 (quotient n (inexact->exact (ceiling (log n))))))


; eof
