;; Given a .lop file and the corresponding .sch file, tells us which globals
;; are referenced but not defined.
;;
;; Useful as a diagnostic tool.
;;
;; Usage: (free-globals schemefilename lopfilename)

(define (globalvars lopfilename)
  (let ((p (open-input-file lopfilename)))
    (let loop ((i (read p)) (l '()))
      (if (eof-object? i)
	  (begin (close-input-port p)
		 (uniquify l))
	  (loop (read p) (crunch (cdr i) l))))))

(define (crunch vec l)
  (let loop ((i 0) (m (vector-length vec)) (l l))
    (if (< i m)
	(let ((item (vector-ref vec i)))
	  (case (car item)
	    ((global)
	     (loop (+ i 1) m (cons (cadr item) l)))
	    ((constvector) 
	     (loop (+ i 1) m (crunch (cadr item) l)))
	    (else (loop (+ i 1) m l))))
	l)))

(define (uniquify l)
  (let loop ((q '()) (l l))
    (cond ((null? l)
	   q)
	  ((memq (car l) q)
	   (loop q (cdr l)))
	  (else
	   (loop (cons (car l) q) (cdr l))))))

(define (globaldefs fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ((i (read)) (l '()))
	(if (eof-object? i)
	    l
	    (if (and (pair? i) (eq? (car i) 'define))
		(if (pair? (cadr i))
		    (loop (read) (cons (caadr i) l))
		    (loop (read) (cons (cadr i) l)))
		(loop (read) l)))))))

(define (free-globals schfile lopfile)

  (define (difference a b)
    (let loop ((a a) (b b) (c '()))
      (cond ((null? a) 
	     c)
	    ((memq (car a) b)
	     (loop (cdr a) b c))
	    (else
	     (loop (cdr a) b (cons (car a) c))))))

  (difference (globalvars lopfile) (globaldefs schfile)))
