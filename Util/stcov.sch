; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test coverage and path profiling for R4RS Scheme programs.
;
; HOW TO USE
; Load this file into a Scheme system.  Evaluate
;
;   (stcov-files '(file1 file2 ...))
;
; This results in file1.stcov, file2.stcov, ... being created.  These
; are identical to the input files except that comments have been stripped,
; formatting has changed (probably), and the code has been anntotated
; with calls to a procedure `$stcov' in strategic places (see below for
; details).  In addition, a file called "stcov-util.sch" will be created
; in the current directory.  
;
; To run the annotated code, first load stcov-util.sch, then all the 
; annotated program files in their usual order, and then run the program.
;
; When the program has finished, the procedure `$stcov-save-results' saves 
; coverage information to a file of your choice.
;
; EXAMPLE
; In your development system:
;
;   > (stcov-files '("File1.sch" "File2.sch"))
;
; Then, in your test system:
;
;   > (load "stcov-util.sch")
;   > (load "File1.sch.stcov")
;   > (load "File2.sch.stcov")
;   > (run-my-program)
;   > ($stcov-save-results "result.stcov")
;
; Back in your development system, analyze the results:
;
;   > (stcov-read-results "result.stcov")
;   > (stcov-summarize-coverage)
;   File1.sch: 29, 30, 36, 37, 38, 40, 47, 53, 54, 55, 56, 57, 58, 59, 
;   60, 61, 77, 94, 98, 103, 137, 142, 143,
;   File2.sch: 161, 171, 177, 179, 184, 189, 199, 200, 205, 209, 212, 215,
;
; The numbers denote control points that were not reached during execution.
; Now look at the transformed source files to map control points back to
; the source code.
;
; BUGS
; Doesn't do quasiquote (handles it, but punts).
; Doesn't do R5RS macros.
; Doesn't deal properly with nonlocal exits.
; 
; DESCRIPTION
; The program inserts a call ($stcov X) where X is a unique identifier 
; in the following places:
;   At the beginning of every body of a LAMBDA or DEFINE.
;   At the beginning of each arm of an IF, COND, or CASE.
;   Before the body in a DO, and before the exit expressions (if any).
;   In the body of every DELAYed expression.
;   Before the second and subsequent conditions in an OR or AND.
;
; The actual code inserted is customizable -- change the procedure
; `stcov-annotation' to return the expression to be inserted.
;
; Except for the problem with generating unique numbers (which may be 
; finessed by generating unique identifiers and quoting them?), the 
; process should be expressible in the high-level macro system with the 
; LET* binding discipline extension.
;
; FURTHER WORK
; The procedure stcov-summarize-execution is an experimental profiling
; facility.  The annotations must be improved: we must deal with join
; points.
;
; The program would be even more useful if it could recognize control 
; points that depend on higher-level control points; this way it 
; could reduce the output by only printing the higher-level control point
; when coverage does not reach a certain part of the program.
;
; Because the program goes to some trouble to ensure that the identifiers
; are generated in a deterministic order, it is possible to write a simple
; program that produces output that's identical (modulo comments
; and formatting) to the input but where nonexecuted expressions are
; indicated visually, eg folded to all-caps, typeset in boldface, etc.
; Think HTML.

(define (stcov-files files)

  (define save-results
    '(define ($stcov-save-results filename)
       (call-with-output-file filename
	 (lambda (out)
	   (display $stcov-ids out)
	   (newline out)
	   (display $stcovv out)
	   (newline out)))))

  (define stcov
    '(define ($stcov x)
       (vector-set! $stcovv x (+ (vector-ref $stcovv x) 1))))

  (reset-stcov)
  (let ((ids '()))
    (do ((files files (cdr files)))
	((null? files))
      (call-with-values 
       (lambda ()
	 (stcov-file (car files)))
       (lambda (first last)
	 (set! ids (cons (list (car files) first last) ids)))))
    (call-with-output-file "stcov-util.sch"
      (lambda (out)
	(pretty-print `(define $stcovv (make-vector ,*stcov-ident* 0)) out)
	(pretty-print `(define $stcov-ids ',(reverse ids)) out)
	(pretty-print stcov out)
	(pretty-print save-results out)))))

(define (stcov-file fn)
  (let ((first-id *stcov-ident*))
    (call-with-input-file fn
      (lambda (in)
	(call-with-output-file (string-append fn ".stcov")
	  (lambda (out)
	    (do ((expr (read in) (read in)))
		((eof-object? expr))
	      (pretty-print (stcov-expr expr) out)
	      (newline out))))))
    (let ((last-id (- *stcov-ident* 1)))
      (values first-id last-id))))

(define stcov-results #f)
(define stcov-ids #f)

(define (stcov-read-results filename)
  (call-with-input-file filename
    (lambda (in)
      (set! stcov-ids (read in))
      (set! stcov-results (read in))
      #t)))

(define (stcov-summarize-coverage)
  (let loop ((i 0) (ids (cons (list "" -1 -1) stcov-ids)))
    (cond ((= i (vector-length stcov-results)))
	  ((zero? (vector-ref stcov-results i))
	   (if (not (<= (cadar ids) i (caddar ids)))
	       (let loop2 ((ids (cdr ids)))
		 (if (not (<= (cadar ids) i (caddar ids)))
		     (loop2 (cdr ids))
		     (begin
		       (newline)
		       (display (caar ids)) (display ": ")
		       (display i)
		       (display ",")
		       (loop (+ i 1) ids))))
	       (begin
		 (display " ")
		 (display i)
		 (display ",")
		 (loop (+ i 1) ids))))
	  (else
	   (loop (+ i 1) ids)))))

(define (stcov-summarize-execution)
  (let ((m (make-vector 20 '(0 . 0))))

    (define (insert i x limit)
      (if (>= limit 0)
	  (let ((y (vector-ref m limit)))
	    (if (> x (car y))
		(begin (vector-set! m (+ limit 1) y)
		       (insert i x (- limit 1)))
		(vector-set! m (+ limit 1) (cons x i))))
	  (vector-set! m (+ limit 1) (cons x i))))

    (let loop ((i 0) (j 0))
      (cond ((= i (vector-length stcov-results))
	     (let ((n (make-vector j)))
	       (do ((k 0 (+ k 1)))
		   ((= k j) n)
		 (vector-set! n k (vector-ref m k)))))
	    ((>= (vector-ref stcov-results i) (car (vector-ref m j)))
	     (insert i (vector-ref stcov-results i) j)
	     (loop (+ i 1) (min (+ j 1) (- (vector-length m) 2))))
	    (else
	     (loop (+ i 1) j))))))

(define (stcov-annotation id)
  `($stcov ,id))

(define (stcov-expr expr)
  (cond ((pair? expr)
	 (case (car expr)
	   ((if) (stcov-if expr))
	   ((lambda) (stcov-lambda expr))
	   ((set!) (stcov-set! expr))
	   ((or and) (stcov-condition expr))
	   ((begin) (stcov-seq expr))
	   ((delay) (stcov-delay expr))
	   ((let let* letrec) (stcov-let expr))
	   ((define) (stcov-define expr))
	   ((cond) (stcov-cond expr))
	   ((case) (stcov-case expr))
	   ((do) (stcov-do expr))
	   ((quasiquote) (stcov-quasiquote expr))
	   ((quote) expr)
	   (else
	    (stcov-call expr))))
	((symbol? expr) expr)
	((or (number? expr)
	     (boolean? expr)
	     (string? expr)
	     (null? expr)
	     (eof-object? expr)
	     (procedure? expr)
	     (char? expr)
	     (eq? expr (unspecified)))
	 expr)
	(else 
	 (error "Unknown form " expr))))

(define (stcov-define expr)
  (if (symbol? (cadr expr))
      (let ((id (next-stcov-ident)))
	`(define ,(cadr expr)
	   (begin ,(stcov-annotation id)
		  ,(stcov-expr (caddr expr)))))
      `(define ,(cadr expr)
	 ,@(stcov-body (cddr expr)))))

; (D ... E ...) ;   => (D ... ($stcov n) E ...)
; where D ... is sequence of definitions of begins that contain definitions
; (recursively).

(define (stcov-body body)

  (define (finish exprs)
    (let ((id (next-stcov-ident)))
      `(,(stcov-annotation id)
	,@(map-l-r stcov-expr exprs))))

    (let loop ((body body))
      (cond ((null? body) 
	     (finish '()))
	    ((pair? (car body))
	     (cond ((eq? (caar body) 'begin)
		    (let ((x `(begin ,@(stcov-body (cdar body)))))
		      (cons x (loop (cdr body)))))
		   ((eq? (caar body) 'define)
		    (let ((x (stcov-define (car body))))
		      (cons x (loop (cdr body)))))
		   (else
		    (finish body))))
	    (else
	     (finish body)))))

(define (stcov-if expr)
  (let* ((then-id (next-stcov-ident))
	 (else-id (next-stcov-ident))
	 (E1 (stcov-expr (cadr expr)))
	 (E2 (stcov-expr (caddr expr))))
    (if (null? (cdddr expr))
	`(if ,E1
	     (begin ,(stcov-annotation then-id) ,E2))
	`(if ,E1
	     (begin ,(stcov-annotation then-id) ,E2)
	     (begin ,(stcov-annotation else-id)
		    ,(stcov-expr (cadddr expr)))))))

(define (stcov-lambda expr)
  `(lambda ,(cadr expr)
     ,@(stcov-body (cddr expr))))

(define (stcov-set! expr)
  `(set! ,(cadr expr) ,(stcov-expr (caddr expr))))

(define (stcov-call expr)
  (map-l-r stcov-expr expr))

(define (stcov-delay expr)
  (let ((id (next-stcov-ident)))
    `(.make-promise (lambda () 
		      ,(stcov-annotation id)
		      ,(stcov-expr (cadr expr))))))

(define (stcov-seq expr)
  `(,(car expr) ,@(map-l-r stcov-expr (cdr expr))))

(define (stcov-condition expr)
  (cond ((null? (cdr expr)) expr)
	((null? (cddr expr))
	 `(,(car expr) ,(stcov-expr (cadr expr))))
	(else
	 (let* ((id (next-stcov-ident))
		(first (stcov-expr (cadr expr)))
		(rest (map-l-r (lambda (x)
				 (let ((id (next-stcov-ident)))
				   `(begin ,(stcov-annotation id)
					   ,(stcov-expr x))))
			       (cddr expr))))
	   `(,(car expr) ,first ,@rest)))))

(define (stcov-let expr)

  (define (fix-bindings bindings)
    (map-l-r (lambda (b)
	       (list (car b) (stcov-expr (cadr b))))
	     bindings))

  (define (fix-body body) 
    (map-l-r stcov-expr body))
    
  (if (symbol? (cadr expr))
      (let* ((bindings (fix-bindings (caddr expr)))
	     (body     (fix-body (cdddr expr))))
	`(let ,(cadr expr) ,bindings ,@body))
      (let* ((bindings (fix-bindings (cadr expr)))
	     (body     (fix-body (cddr expr))))
	`(,(car expr) ,bindings ,@body))))

(define (stcov-cond expr)

  (define (fix-clause x)
    (cond ((eq? (car x) 'else)
	   (let ((id (next-stcov-ident)))
	     `(else 
	       ,(stcov-annotation id)
	       ,@(map-l-r stcov-expr (cdr x)))))
	  ((null? (cdr x))
	   `(,(stcov-expr (car x))))
	  ((eq? (cadr x) '=>)
	   (let* ((lhs (stcov-expr (car x)))
		  (rhs (stcov-expr (caddr x))))
	     `(,lhs => ,rhs)))
	  (else
	   (let* ((test (stcov-expr (car x)))
		  (id  (next-stcov-ident))
		  (exprs (map-l-r stcov-expr (cdr x))))
	     `(,test ,(stcov-annotation id) ,@exprs)))))

  `(cond ,@(map-l-r fix-clause (cdr expr))))

(define (stcov-case expr)

  (define (fix-clause x)
    (cond ((eq? (car x) 'else)
	   (let ((id (next-stcov-ident)))
	     `(else 
	       ,(stcov-annotation id)
	       ,@(map-l-r stcov-expr (cdr x)))))
	  ((null? (cdr x))
	   x)
	  (else
	   (let ((id (next-stcov-ident)))
	     `(,(car x) 
	       ,(stcov-annotation id)
	       ,@(map-l-r stcov-expr (cdr x)))))))

  (let ((e0 (stcov-expr (cadr expr))))
    `(case ,e0 ,@(map-l-r fix-clause (cddr expr)))))

(define (stcov-do expr)

  (define (fix-binding b)
    (if (null? (cddr b))
	`(,(car b) ,(stcov-expr (cadr b)))
	(let* ((init (stcov-expr (cadr b)))
	       (step (stcov-expr (caddr b))))
	  `(,(car b) ,init ,step))))

  (let* ((bindings (map-l-r fix-binding (cadr expr)))
	 (test (stcov-expr (car (caddr expr))))
	 (termination (map-l-r stcov-expr (cdr (caddr expr))))
	 (expanded-termination (if (not (null? termination))
				   `(,(stcov-annotation (next-stcov-ident))
				     ,@termination)
				   '()))
	 (body (map-l-r stcov-expr (cdddr expr)))
	 (expanded-body (if (not (null? body))
			    `(,(stcov-annotation (next-stcov-ident))
			      ,@body)
			    '())))
    `(do ,bindings
	 (,test ,@expanded-termination)
       ,@expanded-body)))
  
; Punt -- fix later.

(define (stcov-quasiquote expr)  expr)


; Left-to-right

(define (map-l-r p l)
  (if (null? l)
      '()
      (let ((x (p (car l))))
	(cons x (map-l-r p (cdr l))))))

(define *stcov-ident* 0)

(define (reset-stcov)
  (set! *stcov-ident* 0))

(define (next-stcov-ident)
  (let ((x *stcov-ident*))
    (set! *stcov-ident* (+ x 1))
    x))

; eof
