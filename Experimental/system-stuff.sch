; Counts all the objects in the heap and returns an association list.
; Under non-exceptional circumstances the "other" counts will be 0.

(define (count-objects-in-heap)
    (define classes
      (list (list pair? 0 'pair)
	    (list vector? 0 'vector)
	    (list port? 0 'port)
	    (list structure? 0 'structure)
	    (list ratnum? 0 'ratnum)
	    (list rectnum? 0 'rectnum)
	    (list symbol? 0 'symbol)
	    (list vector-like? 0 'other-vector-like)
	    (list bytevector? 0 'bytevector)
	    (list string? 0 'string)
	    (list flonum? 0 'flonum)
	    (list bignum? 0 'bignum)
	    (list compnum? 0 'compnum)
	    (list bytevector-like? 0 'other-bytevector-like)
	    (list procedure? 0 'procedure)
	    (list (lambda (x) #t) 0 'other)))
    (let ((x (sro -1 -1 -1)))
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length x)))
	(let ((y (vector-ref x i)))
	  (do ((c classes (cdr c)))
	      (((caar c) y)
	       (set-car! (cdar c) (+ (cadar c) 1))))))
      (map (lambda (x) (list (caddr x) (cadr x))) classes)))

; Garbage Collect Truly Worthless Atoms.

(define (gctwa)

  (define (symbol.proplist s)
    (vector-like-ref s 2))

  (define (symbol<? a b)
    (string<? (symbol->string a) (symbol->string b)))

  (let ((symbols (sro 3 (typetag 'gctwa) 1)))
    (do ((i 0 (+ i 1))
	 (dead '()
	       (let ((s (vector-ref symbols i)))
		 (if (null? (symbol.proplist s))
		     (cons s dead)
		     dead))))
	((= i (vector-length symbols))
	 (oblist-set! (select (lambda (x)
				(not (memq x dead)))
			      (oblist)))))))

; eof

