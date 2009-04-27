;For PLT Scheme 4.1.5 using plt-r5rs 

;(require (lib "1.ss" "srfi"))
(require (lib "9.ss" "srfi"))
(require (lib "23.ss" "srfi"))
(require (lib "60.ss" "srfi"))
(require (lib "66.ss" "srfi"))
(require (lib "69.ss" "srfi"))

(load "other/srfi-60-pieces.scm")

(define (require-4.x-id require-spec id)
  (let ((ns0 (make-base-namespace)))
    (eval `(require (only-in ,require-spec ,id)) ns0)
    (eval id ns0)))

(define alist->hash-table
  (let* ((al->h-t alist->hash-table)
	 (i-cons (require-4.x-id 'scheme 'cons))
	 (mp->ip
	  (lambda (p)
	    (i-cons (car p) (cdr p))))
	 (mal->ial
	  (lambda (al)
	    (let rec ((al al))
	      (cond ((null? al) al)
		    (else (i-cons (mp->ip (car al))
				  (rec (cdr al)))))))))
    (lambda (al)
      (al->h-t (mal->ial al)))))
