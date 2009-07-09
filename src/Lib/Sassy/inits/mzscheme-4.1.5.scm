;For PLT Scheme 4.1.5 using plt-r5rs 

(require (lib "9.ss" "srfi"))
(require (lib "23.ss" "srfi"))
(require (lib "60.ss" "srfi"))
(require (lib "66.ss" "srfi"))
(require (lib "69.ss" "srfi"))

(load "other/srfi-60-pieces.scm")

(define (require-4.x-id require-spec id)
  (let ((ns0 (make-namespace)))
    (eval `(require ,require-spec) ns0)
    (eval id ns0)))

;; captures common pattern; hopefully "good enough"
;; for all cases of interest...
;; (note that the marshalling is shallow.)

;; marshalling : FcnName[X ... -> Y ...] -> (X ... -> Y ...)

(define (marshalling-srfi-1-variant name)
  (let* ((orig-fcn (require-4.x-id 'srfi/1 name))
	 (mlist->ilist (require-4.x-id 'scheme/mpair 'mlist->list))
	 (ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist))
	 (mpair?       (require-4.x-id 'scheme 'mpair?))
	 (ipair?       (require-4.x-id 'scheme 'pair?))
	 (i-marshall
	  (lambda (val)
	    (if (mpair? val) (mlist->ilist val) val)))
	 (m-marshall
	  (lambda (val)
	    (if (ipair? val) (ilist->mlist val) val))))
    (lambda args
      (call-with-values (lambda () 
			  (apply orig-fcn (map i-marshall args)))
	(lambda results
	  (apply values (map m-marshall results)))))))
	

(define span
  (let ((i-span (require-4.x-id 'srfi/1 'span))
	(mlist->ilist (require-4.x-id 'scheme/mpair 'mlist->list))
	(ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist)))
    (lambda (f l)
      (call-with-values (lambda () (i-span f (mlist->ilist l)))
	(lambda (b r)
	  (values (ilist->mlist b)
		  (ilist->mlist r)))))))

(define every 
  (let ((i-every (require-4.x-id 'srfi/1 'every))
	(mlist->ilist (require-4.x-id 'scheme/mpair 'mlist->list)))
    (lambda (pred . lists)
      (apply i-every pred (map mlist->ilist lists)))))

(define make-list
  (let ((i-make-list (require-4.x-id 'srfi/1 'make-list))
	(ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist)))
    (lambda args
      (ilist->mlist (apply i-make-list args)))))
    
(define list-copy
  (let ((i-list-copy (require-4.x-id 'srfi/1 'list-copy))
	(mlist->ilist (require-4.x-id 'scheme/mpair 'mlist->list))
	(ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist)))
    (lambda (l)
      (ilist->mlist (i-list-copy (mlist->ilist l))))))


(define map-in-order
  (marshalling-srfi-1-variant 'map-in-order))

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
