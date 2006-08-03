;For gosh (gauche) 0.8.5
(use srfi-1)
(use srfi-9)

(define hash-table-set! hash-table-put!)
(define (hash-table-ref k v . opt)
  (cond ((hash-table-get k v #f))
	(else (if (null? opt)
		  (error "key not in hash-table " k v)
		  (apply (car opt) '())))))
(define (alist->hash-table alst)
  (let ((t (make-hash-table)))
    (for-each (lambda (x)
		(hash-table-set! t (car x) (cdr x)))
	      alst)
    t))

(let* ((old-make-hash-table make-hash-table)
       (new (lambda x
	      (if (null? x)
		  (old-make-hash-table)
		  (old-make-hash-table
		   (let ((t (car x)))
		     (cond ((eq? t eq?) 'eq?)
			   ((eq? t eqv?) 'eqv?)
			   ((eq? t equal?) 'equal?)
			   ((eq? t string=?) 'string=?)
			   (else (error "bad arg to make-hash-table" t)))))))))
  (set! make-hash-table new))
  

(define delete-file sys-remove)
(define getenv      sys-getenv)
