; Copyright 1998, 2004 Lars T Hansen.
;
; $Id$
;
; Larceny library -- application hooks: generalized procedures.
;
; The concept comes from MIT Scheme (at least in my universe), as do the
; names.  An "apply hook" is an object that can be applied like a procedure.
; It contains a procedure and an "extra" object.  When an apply hook is 
; applied, the procedure stored in the hook is called with the arguments 
; given to the hook.
;
;   (MAKE-APPLY-HOOK proc obj) => apply-hook
;   (APPLY-HOOK? obj) => boolean
;   (APPLY-HOOK-PROCEDURE apply-hook) => proc
;   (APPLY-HOOK-EXTRA apply-hook) => obj
;   (SET-APPLY-HOOK-PROCEDURE! apply-hook proc)
;   (SET-APPLY-HOOK-EXTRA! apply-hook obj)
;
; MIT Scheme also has the notion of 'entities', which I have not yet
; implemented, but they are similar.
;
; This version avoids the clean low-level support code through the use
; of some nasty representation manipulation, and we lose a little
; performance in the process, but this code can be loaded directly
; into the interpreter and the other code cannot.
;
; This code is vulnerable to other code that also manipulates the
; procedure representation, because it needs to index from the end of
; a procedure to find its values.

; This belongs in Lib/typetags.sch
(define sys$tag.applyhook-typetag 0)

(define make-apply-hook 
  (let ()

    (define (extend-apply-hook self proc obj)
      (let* ((k (procedure-length self))
	     (new-self (make-procedure (+ k 2))))
	(do ((i 0 (+ i 1)))
	    ((= i k)
	     (procedure-set! new-self k proc)
	     (procedure-set! new-self (+ k 1) obj)
	     (values new-self k))
	  (procedure-set! new-self i (procedure-ref self i)))))

    (define (make-apply-hook-n proc object)
      (letrec ((offs 0)
	       (self (lambda args
		       (apply (procedure-ref self offs) args))))
	(let-values (((new-self proc-offset)
		      (extend-apply-hook self proc object)))
	  (let ((old-self self))
	    (set! offs proc-offset)
	    (set! self new-self)
	    new-self))))

    (lambda (proc object)
      (let ((a (procedure-arity proc)))
	(let ((h (make-apply-hook-n proc object)))
	  (typetag-set! h sys$tag.applyhook-typetag)
	  h)))))

(define (apply-hook? obj)
  (and (procedure? obj)
       (= (typetag obj) sys$tag.applyhook-typetag)))

(define (apply-hook-procedure obj)
  (if (apply-hook? obj)
      (procedure-ref obj (- (procedure-length obj) 2))
      (error "apply-hook-procedure: " obj " is not an apply hook.")))

(define (set-apply-hook-procedure! obj proc)
  (if (apply-hook? obj)
      (procedure-set! obj (- (procedure-length obj) 2) proc)
      (error "set-apply-hook-procedure!: " obj " is not an apply hook.")))

(define (apply-hook-extra obj)
  (if (apply-hook? obj)
      (procedure-ref obj (- (procedure-length obj) 1))
      (error "apply-hook-extra: " obj " is not an apply hook.")))

(define (set-apply-hook-extra! obj val)
  (if (apply-hook? obj)
      (procedure-set! obj (- (procedure-length obj) 1) val)
      (error "set-apply-hook-extra!: " obj " is not an apply hook.")))

; eof 
