; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Sundry system-level code.

(define (ptrtag obj)
  (cond ((pair? obj) 1)
	((vector-like? obj) 3)
	((bytevector-like? obj) 5)
	((procedure? obj) 7)
	(else (error "ptrtag: " obj " doesn't have a pointer tag."))))

(define (boxed? obj)
  (or (pair? obj)
      (vector-like? obj)
      (bytevector-like? obj)
      (procedure? obj)))

; Returns an object's machine representation as an exact integer.
; GC may subsequently invalidate the representation of any boxed structure.

(define (object-representation obj)
  (let ((syscall:object->address 36))
    (cond ((char? obj) (+ (* (char->integer obj) 65536) #x26))
	  ((fixnum? obj) (* obj 4))
	  ((null? obj) 10)
	  ((eq? obj #t) 6)
	  ((eq? obj #f) 2)
	  ((eq? obj (undefined)) #x316)
	  ((eq? obj (unspecified)) #x116)
	  ((eof-object? obj) #x216)
	  ((boxed? obj)
	   ((system-function 'syscall) syscall:object->address obj))
	  (else (error "representation: " obj " is unknown!")))))

; Return an object's size as the number of bytes of memory it occupies,
; not taking its deep structure into account.  Immediate objects have
; size 0 (they can only exist inside other objects).

(define (object-size obj)

  (define (roundup8 n)
    (* 8 (quotient (+ n 7) 8)))

  (cond ((pair? obj) 8)
	((vector-like? obj) 
	 (roundup8 (+ 4 (* (vector-like-length obj) 4))))
	((bytevector-like? obj)
	 (roundup8 (+ 4 (bytevector-like-length obj))))
	((procedure? obj)
	 (roundup8 (+ 4 (* (procedure-length obj) 4))))
	(else
	 0)))

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

; Like count-objects-in-heap but computes bytes rather than number.

(define (count-bytes-in-heap)
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
	     (set-car! (cdar c) (+ (cadar c) (object-size y)))))))
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
	 (oblist-set! (filter (lambda (x)
				(not (memq x dead)))
			      (oblist)))))))

; Display amount of space available.

(define (room)

  (define bits-per-byte 8)

  (define wordsize
    (quotient (cdr (assq 'architecture-word-size (system-features)))
	      bits-per-byte))

  (define (memstats-gen x) (vector-ref x 7))

  (define (gen-collections x) (vector-ref x 0))
  (define (gen-promotions x) (vector-ref x 1))
  (define (gen-gctime x) (vector-ref x 2))
  (define (gen-alloc x) (vector-ref x 8))
  (define (gen-use x) (vector-ref x 3))
  (define (gen-size x) (vector-ref x 9))
  
  (let* ((m (memstats))
	 (g (memstats-gen m)))
    (do ((i 0 (+ i 1)))
	((= i (vector-length g)))
      (let ((x (vector-ref g i)))
	(format #t "Generation ~a: size ~a, free ~a, memory ~a~%"
		i
		(* wordsize (gen-size x))
		(* wordsize (- (gen-size x) (gen-use x)))
		(* wordsize (gen-alloc x)))))))

; eof
