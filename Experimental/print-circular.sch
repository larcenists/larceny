; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; A printer for shared and circular structures.
;
; The syntax #n=<object> associates mark n with <object>.
; The syntax #n# is a reference to the object associated with mark n.

(define (print-circular obj . rest)
  (print-circular-internal obj
			   (current-output-port)
			   (if (null? rest) #f (car (rest)))
			   (lambda (obj port quote?)
			     (if quote?
				 (write obj port)
				 (display obj port)))))

; Primitive-printer is the system's print procedure: it should be used 
; to print objects that are not mutable or that do not have an interesting
; (i.e., readable) printed representation, e.g., procedures print as 
; #<procedure x>, which is uninteresting.
;
; The circular-printer assumes that only pairs, vectors, and strings
; are mutable objects with an interesting printed representation.  This
; is false, since at least structures can be printed in interesting 
; ways.  The definition of installed structure printers needs to be
; fixed to deal with this issue, maybe.

(define (print-circular-internal obj port quote? primitive-printer)

  (define tbl                           ; Maps objects to refs 
    (make-hashtable equal-hash assq))

  (define next 0)			; Next output ID to be used

  (define (make-ref obj) (list obj 1 #f)))

  (define (ref-increment-count! r)
    (set-car! (cdr r) (+ (cadr r) 1)))

  (define (ref-count r) (cadr r))

  (define (ref-already-printed? r)      ; Has been printed if seq number not #f
    (caddr r))

  (define (ref-sequence-number r)       ; Return a sequence number
    (let ((x (caddr r)))
      (if x
	  x
	  (let ((x next))
	    (set! next (+ next 1))
	    (set-car! (cddr r) x)
	    x))))

  (define (out x quote?)
    (primitive-printer x port quote?))

  ; Must add objects that have identity to the hash table.  Procedures,
  ; bytevectors, ports, structures, and environments have identity, too, 
  ; but they can't be read, so it doesn't matter.

  (define (preprocess x)
    (if (or (pair? x) (vector? x) (string? x))
	(let ((r (hashtable-get tbl x)))
	  (if (not r)
	      (begin (hashtable-put! tbl x (make-ref x))
		     (cond ((pair? x)
			    (preprocess (car x))
			    (preprocess (cdr x)))
			   ((vector? x)
			    (do ((i (- (vector-length x) 1) (- i 1)))
				((< i 0))
			      (preprocess (vector-ref x i))))))
	      (ref-increment-count! r)))))

  (define (print-pair obj)
    (out "(" #f)
    (print-pair2 obj)
    (out ")" #f))

  (define (print-pair2 obj)
    (print (car obj))
    (let ((x (cdr obj)))
      (cond ((null? x))
            ((pair? x)
	     (let ((r (hashtable-get tbl x)))
	       (cond ((ref-already-printed? r)
                      (out " . " #f)
                      (print-reference r))
                     ((> (ref-count r) 1)
                      (out " . " #f)
                      (print-mark r)
                      (print-pair x))
                     (else
                      (out " " #f)
                      (print-pair2 x)))))
	    (else
	     (out " . " #f)
	     (print x)))))
	    
  (define (print-vector v)
    (out "#(" #f)
    (do ((i 0 (+ i 1)))
	((= i (vector-length v)))
      (print (vector-ref v i))
      (if (< i (- (vector-length v) 1))
	  (out " " #f)))
    (out ")" #f))

  (define (print-reference r)
    (out "#" #f)
    (out (ref-sequence-number r) #f)
    (out "#" #f))

  (define (print-mark r)
    (out "#" #f)
    (out (ref-sequence-number r) #f)
    (out "=" #f))

  (define (print-obj-with-identity x print-obj)
    (let ((r (hashtable-get tbl x)))
      (cond ((ref-already-printed? r)
	     (print-reference r))
	    ((> (ref-count r) 1)
	     (print-mark r)
	     (print-obj x))
	    (else
	     (print-obj x)))))

  (define (print obj)
    (cond ((pair? obj)
	   (print-obj-with-identity obj print-pair))
	  ((vector? obj)
	   (print-obj-with-identity obj print-vector))
	  ((string? obj)
	   (print-obj-with-identity obj (lambda (x) (out x quote?))))
	  (else
	   (out obj quote?))))
  
  (preprocess obj)
  (print obj))

; eof
