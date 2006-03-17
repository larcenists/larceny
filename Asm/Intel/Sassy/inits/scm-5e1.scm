;For scm-5e1 with SLIB 3a2

(define annoying-eval eval)
(define (interaction-environment) 'whatever)
(define (eval e i) (annoying-eval e))

(require 'common-list-functions)
(require 'srfi-9)
(require 'hash-table)
(require 'byte)

;==================================;
; 				   ;
; SRFI 69 basic hash-tables	   ;
; 				   ;
;==================================;
(define old-make-hash make-hash-table)
(define (make-hash-table) (old-make-hash 997))

(define hash-table-set!
  (let ((setter (hash-associator equal?)))
    (lambda (t k v)
      (setter t k v))))

(define hash-table-ref
  (let ((getter (hash-inquirer equal?)))
    (lambda (t k . opt)
      (cond ((getter t k))
	    (else (if (null? opt)
		      (error "key not in hash-table" k v)
		      (apply (car opt) '())))))))

(define (alist->hash-table alst)
  (let ((t (make-hash-table)))
    (for-each (lambda (kv)
		(hash-table-set! t (car kv) (cdr kv)))
	      alst)
    t))

(define (hash-table-values table)
  (let ((result '()))
    (hash-for-each (lambda (k v)
		     (set! result (cons v result)))
		   table)
    result))

(define (hash-table-keys table)
  (let ((result '()))
    (hash-for-each (lambda (k v)
		     (set! result (cons k result)))
		   table)
    result))


; not totally correct but good enough for here
; (SLIB defines a hash-table to be a vector of alists)
(define hash-table? vector?)
