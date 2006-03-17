;For gosh (gauche) 0.8.5

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

(define (delete-file sys-remove))
