; Larceny library -- namespaces.
; $Id$

(define (make-namespace . rest) 
  (cond ((null? rest)
	 (vector namespace-tag (make-hash-table eq?) #f))
	((null? (cdr rest))
	 (vector namespace-tag (make-hash-table eq?) (car rest)))
	(else
	 (error "make-namespace: too many arguments: " rest))))

(define (namespace? obj) 
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) namespace-tag)))

;Internal
(define (namespace-ref ns sym) 
  (hash-table-ref (vector-ref ns 1) sym))

;Internal
(define (namespace-set! ns sym loc) 
  (hash-table-set! (vector-ref ns 1) sym loc))

;Internal
(define (make-global-cell val sym) 
  (cons val sym))

;Internal
(define (global-cell-ref loc) 
  (car loc))

;Internal
(define (global-cell-set! loc)
  (cdr loc))


(define (namespace-add-value! ns sym val)
  (namespace-set! ns sym (make-global-cell val sym)))

(define (namespace-get-value ns sym)
  (let ((probe (namespace-ref ns sym)))
    (if probe
	(global-cell-ref probe)
	(let ((super (vector-ref ns 2)))
	  (if super
	      (namespace-get-value super sym)
	      (undefined))))))

(define (namespace-get-cell ns sym)
  (let ((probe (namespace-ref ns sym)))
    (if probe
	probe
	(let ((v (namespace-get-value ns sym)))
	  (namespace-set! ns sym v)
	  (namespace-ref ns sym)))))

(define current-namespace
  (let ((ns #f))
    (lambda rest
      (cond ((null? rest) ns)
	    ((null? (cdr rest)) 
	     (let ((old ns))
	       (set! ns (car rest))
	       old))
	    (else
	     (error "current-namespace: too many arguments: " rest))))))

(define (with-namespace new-ns thunk)
  (let ((old-ns (current-namespace)))
    (dynamic-wind (lambda ()
		    (current-namespace new-ns))
		  thunk
		  (lambda ()
		    (current-namespace old-ns)))))

(define (namespace-copy ns) 
  (call-without-interrupts
   (lambda ()
     (let ((new (make-hash-table eq?)))
       (hash-table-for-each (vector-ref ns 1) 
			    (lambda (key val)
			      (hash-table-set! new key val)))
       new))))

(define r4rs-namespace
  (let ((+ +)
	(- -)
	(* *)
	(/ /)
	...)
    (lambda ()
      (let ((ns (make-namespace)))
	(namespace-add-value! ns '+ +)
	...
	ns))))

(define larceny-namespace
  (let ((sort sort)
	...)
    (lambda ()
      (let ((ns (r4rs-namespace)))
	(namespace-add-value! ns 'sort sort)
	...
	ns))))

(define eval 
  (let ((eval eval))
    (lambda (expr . rest)
      (if (null? rest)
	  (eval expr)
	  (with-namespace (car rest) (lambda () (eval expr)))))))

(define load
  (let ((load load))
    (lambda (filename . rest)
      (if (null? rest)
	  (load filename)
	  (with-namespace (car rest) (lambda () (load filename)))))))

; eof 
