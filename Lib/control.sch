; Copyright 1991 Lightship Software
; Copyright 1995 Lars Thomas Hansen
;
; Larceny library -- various control procedures
;
; $Id: control.sch,v 1.1 1995/08/03 00:18:21 lth Exp lth $
;
; Member, assq, and so on may be found in list.sch.
; Call-with-current-continuation is in malcode.mal.
; Exit is OS-specific (see e.g. unix.sch)


; APPLY
;
; @raw-apply@ is written in mal (MacScheme assembly language)
; in another file.  It takes exactly three arguments and does
; something magical with them.
;
; (list? l) and (length l) can be computed in one pass over the list,
; if performance is a problem here.

(define apply
  (let ((raw-apply @raw-apply@))              ; see above

    (define (loop x rest)
      (if (null? (cdr rest))
          (append (reverse x) (car rest))
          (loop (cons (car rest) x) (cdr rest))))

    (define (apply f l . rest)
      (cond ((not (procedure? f))
             (error "Non-procedure -- apply" f))
            ((not (null? rest))
             (apply f (loop (list l) rest)))
            ((null? l) (f))
            ((null? (cdr l)) (f (car l)))
            ((null? (cddr l)) (f (car l) (cadr l)))
            ((null? (cdddr l)) (f (car l) (cadr l) (caddr l)))
            ((null? (cddddr l)) (f (car l) (cadr l) (caddr l) (cadddr l)))
            ((not (list? l))
             (error "Non-list -- apply" l))
            (else (raw-apply f l (length l)))))

    apply))

; from make25.sch

(define call-without-interrupts
  (lambda (thunk)
    (thunk)))

; Used by 'delay'.

(define %make-promise
  (lambda (proc)
    (let ((result-ready? #f)
	  (result        #f))
      (lambda ()
	(if result-ready?
	    result
	    (let ((x (proc)))
	      (if result-ready?
		  result
		  (begin (set! result-ready? #t)
			 (set! result x)
			 result))))))))

(define force
  (lambda (proc)
    (proc)))


; Garbage collection.

(define (collect . args)
  (let ((type $gc.ephemeral))
    (if (not (null? args))
	(let ((a (car args)))
	  (cond ((eq? a 'ephemeral) (set! type $gc.ephemeral))
		((eq? a 'tenuring)  (set! type $gc.tenuring))
		((eq? a 'full)      (set! type $gc.full))
		(else
		 (error "collect: bad argument "
			(car args)
			#\newline
			"use one of 'ephemeral, 'tenuring, 'full.")))))
    (sys$gc type)))


; Given a continuation (which is a procedure), return the internal 
; linked list of frames (represented as vectors).  This is useful for 
; debuggers.
;
; See malcode.mal for sys$continuation-data-structure.
;
; NOTE that this procedure is pretty much useless for programs that
; wrap continuations in other procedures; for an example of code that
; does this, see the definition of dynamic-wind below.  We can work 
; around this in the specific case of dynamic-wind by some malcode
; hackery but I have not done so, as the solution is nowhere general.

(define (continuation->vector obj)
  (if (continuation? obj)
      (sys$continuation-data-structure obj)
      (error "continuation->vector: not a continuation: " obj)))


; For now continuations are "plain" procedures and this primitive does
; not perform the necessary tests; see malcode.mal.

(define continuation? procedure?)


; dynamic-wind
;
; Snarfed from Lisp Pointers, V(4), October-December 1992, p45.
; Written by Jonathan Rees.
;
; NOTE: this messes up the continuation->vector procedure, above,
; making the latter useless.
;
; NOTE: this code is not thread-aware.

(define *here* (list #f))

(define call-with-current-continuation 
  (let ((call-with-current-continuation call-with-current-continuation))
    (lambda (proc)
      (let ((here *here*))
	(call-with-current-continuation
	 (lambda (cont)
	   (proc (lambda results
		   (reroot! here)
		   (apply cont results)))))))))
    
(define (dynamic-wind before during after)
  (let ((here *here*))
    (reroot! (cons (cons before after) here))
    (let ((r (during)))
      (reroot! here)
      r)))

; The code following the call to reroot! is really the following, but
; we don't have multiple values yet:
;
;    (call-with-values during
;      (lambda results
;	(reroot! here)
;	(apply values results)))

(define (reroot! there)
  (if (not (eq? *here* there))
      (begin (reroot! (cdr there))
	     (let ((before (caar there))
		   (after  (cdar there)))
	       (set-car! *here* (cons after before))
	       (set-cdr! *here* there)
	       (set-car! there #f)
	       (set-cdr! there '())
	       (set! *here* there)
	       (before)))))


; Install procedure which resolves global names in LOAD and EVAL.

(define global-name-resolver
  (let ((p (lambda (sym)
	     (error "global-name-resolver: not installed."))))
    (lambda rest
      (cond ((null? rest) p)
	    ((and (null? (cdr rest))
		  (procedure? (car rest)))
	     (let ((old p))
	       (set! p (car rest))
	       old))
	    (else
	     (error "global-name-resolver: Wrong number of arguments: "
		    rest))))))


; eof
