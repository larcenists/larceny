; This program reads a log _after_ hand-cleaning (sigh) and prints
; out the maxheap fields from each run.
;
; The cleanup must:
;  remove larceny's banner and prompts
; at least; I can't remember what else.  For the most part the benchmarks
; don't print anything, but if they do, they should precede the output
; by a semicolon.
;
; The banner and prompts may be gotten rid of with judicious use of -q and
; repl-prompt.


(define (doit file)
  (max-maxheap-all (read-log file)))

(define (read-log file)
  (call-with-input-file file
    (lambda (in)
      (partition (slurp in)))))

(define (max-maxheap-all items)
  (map extract-maxheap items))

(define (slurp in)
  (let ((x (read in)))
    (if (eof-object? x)
	'()
	(begin
	  (if (vector? x)
	      (begin (display x)
		     (newline)))
	  (cons x (slurp in))))))

(define (partition l)
  (let loop ((l l) (r '()))
    (cond ((null? l) (reverse r))
	  ((vector? (car l))
	   (let loop2 ((l2 (cdr l)) (r2 (list (car l))))
	     (if (or (null? l2)
		     (vector? (car l2)))
		 (loop l2 (cons (reverse r2) r))
		 (loop2 (cdr l2) (cons (car l2) r2)))))
	  (else
	   (display "Bogosity ")
	   (display (car l))
	   (loop (cdr l) r)))))

(define (extract-maxheap item)

  (define (find-subruns x)
    (cond ((null? x) '())
	  ((and (pair? (car x)) (eq? 'RUNNING (caar x)))
	   (cons (car x) (find-subruns (cdr x))))
	  (else
	   (find-subruns (cdr x)))))

  (define (find x l)
    (cond ((null? l) #f)
	  ((and (pair? (car l)) (eq? x (caar l))) (car l))
	  (else (find x (cdr l)))))

  (define (strip-gunk l)
    (cond ((null? l) l)
	  ((pair? (car l)) l)
	  (else (strip-gunk (cdr l)))))

  (define (extract x)
    (if (not (eq? (car x) 'running))
	(error "No RUNNING"))
    (let ((probe (find 'maxheap (car (strip-gunk (cddr x))))))
      (if probe
	  (cadr probe)
	  (car (last-pair (map extract (find-subruns x)))))))

  (if (not (vector? (car item)))
      (error "No vector"))
  (cons (car item) (map extract (cdr item))))

; eof
