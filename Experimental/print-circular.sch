; Experimental/print-circular.sch
; Larceny -- a printer for circular objects
;
; $Id$
;
; It's currently O(n^3), which is not at all good.  It can be O(n) if
; we can implement an O(1) object-id or object-hash procedure.  We'd
; then have an internal actual hash table keyed on object ID.
;
; But if you're willing to pay, you can play.

; Test code

(define (print-circular obj . rest)
  (print-circular-internal obj
			   (current-output-port)
			   (if (null? rest) #f (car (rest)))
			   (lambda (obj port quote?)
			     (if quote?
				 (write obj port)
				 (display obj port)))))

; The idea here is that primitive-printer is the system's print procedure:
; it should be used to print objects that are not mutable or that
; do not have an interesting (i.e., readable) printed representation, 
; e.g., procedures print as #<procedure x>, which is uninteresting.
;
; The circular-printer assumes that only ports, vectors, and strings
; are mutable objects with an interesting printed representation.  This
; is false, since at least structures can be printed in interesting 
; ways.  The definition of installed structure printers needs to be
; fixed to deal with this issue, maybe.

(define (print-circular-internal obj port quote? primitive-printer)

  (define hashtbl '())			; An assoc list (object ID,count,ID)
  (define next 0)			; Next output ID to be used

  ; Return obj or an integer -- the index

  (define (find-reference obj)
    (assq obj hashtbl))

  (define (add-reference obj)
    (set! hashtbl (cons (list obj 1 #f) hashtbl)))

  (define (add-reference-count r)
    (set-car! (cdr r) (+ (cadr r) 1)))

  (define (multiple-references? r)
    (> (cadr r) 1))

  (define (mark-sequence-number r)
    (let ((x (caddr r)))
      (if x
	  x
	  (let ((x next))
	    (set! next (+ next 1))
	    (set-car! (cddr r) x)
	    x))))

  (define (already-printed? r)
    (caddr r))

  (define (out x quote?)
    (*primitive-print* x port quote?))

  ; Must add objects that have identity to the hash table.  Procedures,
  ; bytevectors, ports, structures, and environments have identity, too, 
  ; but they can't be read, so it doesn't matter.

  (define (preprocess x)
    (if (or (pair? x) (vector? x) (string? obj))
	(let ((r (find-reference x)))
	  (if (not r)
	      (begin (add-reference x)
		     (cond ((pair? x)
			    (preprocess (car x))
			    (preprocess (cdr x)))
			   ((vector? x)
			    (do ((i (- (vector-length x) 1) (- i 1)))
				((< i 0))
			      (preprocess (vector-ref x i))))))
	      (add-reference-count r)))))

  (define (print-pair obj)
    (out "(" #f)
    (print-pair2 obj)
    (out ")" #f))

  (define (print-pair2 obj)
    (print (car obj))
    (let ((x (cdr obj)))
      (cond ((pair? x)
	     (let ((r (find-reference x)))
	       (cond ((already-printed? r)
		      (begin (out " . " #f)
			     (print-reference r)))
		     ((not (pair? x))
		      (out " . " #f)
		      (print x))
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
    (out (mark-sequence-number r) #f)
    (out "#" #f))

  (define (print-mark r)
    (out "#" #f)
    (out (mark-sequence-number r) #f)
    (out "=" #f))

  (define (print-obj-with-identity x print-obj)
    (let ((r (find-reference x)))
      (cond ((already-printed? r)
	     (print-reference r))
	    ((multiple-references? r)
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
	   (print-obj-with-identity obj display))
	  (else
	   (out obj quote?))))
  
  (preprocess obj)
  (print obj))

; eof
