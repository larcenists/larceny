; Unix system calls and some fun things built on top of them.

; execl(2)
;
; Usage:
;   (execl path arg ...)
; where path and each arg is a string, e.g.,
;   > (execl "/usr/bin/ksh" "ksh" "-c" "echo Hello, sailor!")
;   Hello, sailor!
;
;   Process scheme finished
;
; The cache is a hack that is only necessary because foreign functions
; are not yet weakly held, and because any use of vfork(2) will actually
; cause foreign functions to be accumulated in the parent's address space.
; (At least I think so; this should be checked by looking at the list of
; foreign functions.  In any event it's a general device for varargs ffs.)
; If one were to use fork(), this would not be a problem since a process
; can call execl() at most once.

(define execl
  (let ((cache '#()))
    (lambda (path . args)
      (let ((l (length args)))
	(if (>= l (vector-length cache))
	    (let ((new-cache (make-vector (+ l 1) #f)))
	      (do ((i 0 (+ i 1)))
		  ((= i (vector-length cache)))
		(vector-set! new-cache i (vector-ref cache i)))
	      (set! cache new-cache)))
	(if (not (vector-ref cache l))
	    (vector-set! cache l
			 (foreign-procedure
			  "execl" (make-list (+ l 2) 'string) 'int)))
	(apply (vector-ref cache l) path (append! args '(#f)))))))

(define vfork
  (foreign-procedure "vfork" '() 'int))

