; Copyright 1991 Lightship Software
;
; $Id$
;
; Larceny library -- some control procedures.
;
; 'Call-with-current-continuation' is in malcode.mal.
; 'Exit' is OS-specific (see e.g. unix.sch)
; 'Call-without-interrupts' is in timer.sch.

($$trace "control")

; APPLY
;
; @raw-apply@ is written in MacScheme assembly language; see Lib/malcode.mal.
; It takes exactly three arguments: a procedure, a list, and the list length.
;
; (list? l) and (length l) can be computed in one pass over the list,
; if performance is a problem here.

(define apply
  (let ((raw-apply @raw-apply@))              ; see above

    (define (collect-arguments l)
      (if (null? (cdr l))
	  (car l)
	  (cons (car l) (collect-arguments (cdr l)))))

    (define (apply f l . rest)
      (cond ((not (procedure? f))
             (error "apply: not a procedure: " f)
	     #t)
            ((not (null? rest))
	     (apply f (cons l (collect-arguments rest))))
            ((null? l) (f))
            ((null? (cdr l)) (f (car l)))
            ((null? (cddr l)) (f (car l) (cadr l)))
            ((null? (cdddr l)) (f (car l) (cadr l) (caddr l)))
            ((null? (cddddr l)) (f (car l) (cadr l) (caddr l) (cadddr l)))
            ((not (list? l)) 
	     (error "apply: not a list: " l)
	     #t)
            (else (raw-apply f l (length l)))))

    apply))


; Used by 'delay'.

(define make-promise
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
; Arguments are from the following groups:
;   {'ephemeral 'tenuring 'full} are largely compatible with pre-v0.26
;   a generation number by itself means collect in that generation
;   a generation number followed by 'collect or 'promote means either
;   collect in that generation or promote into that generation.

(define (collect . args)

  (define (err)
    (error "collect: bad arguments "
	   args
	   #\newline
	   "Use: (collect [generation [{promote,collect}]])")
    #t)

  (if (null? args)
      (sys$gc 0 0)
      (let ((a (car args)))
	(cond ((eq? a 'ephemeral) (sys$gc 0 0))
	      ((eq? a 'tenuring)  (sys$gc 1 1))
	      ((eq? a 'full)      (sys$gc 1 0))
	      ((and (fixnum? a) (positive? a))
	       (if (null? (cdr args))
		   (sys$gc a 0)
		   (let ((b (cadr args)))
		     (cond ((eq? b 'collect) (sys$gc a 0))
			   ((eq? b 'promote) (sys$gc a 1))
			   (else (err))))))
	      (else (err)))))
  (unspecified))


; Returns the current continuation structure (as chain of vectors).

(define (current-continuation-structure)
  (creg))


; From: William D Clinger <will@ccs.neu.edu>
; Newsgroups: comp.lang.scheme
; Subject: Re: call-with-values: what's all about?
; Date: Tue, 11 Nov 1997 15:29:31 -0500

; This is correct for correct programs.
;
; Returning multiple values to a continuation that does not expect them
; (an error) will not be detected.
;
; We need to replace these definitions when Larceny gets compiler 
; support for them.

(define *multiple-values* (list '*multiple-values*))
(define *values0* (list *multiple-values*))

(define values
  (lambda vals
    (cond ((null? vals) *values0*)
	  ((null? (cdr vals)) (car vals))
	  (else (cons *multiple-values* vals)))))

(define call-with-values
  (lambda (producer consumer)
    (let ((vals (producer)))
      (if (and (pair? vals)
	       (eq? (car vals) *multiple-values*))
	  (apply consumer (cdr vals))
	  (consumer vals)))))


; dynamic-wind
;
; Snarfed from Lisp Pointers, V(4), October-December 1992, p45.
; Written by Jonathan Rees.
;
; Modification:  We would like reroot! to be atomic wrt timer interrupts.
; While it is not desirable to disable interrupts while rerooting, since
; buggy user code can then hang the system, I have chosen to do for the
; time being.  Revisit when we start worrying about threads for real.

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
    (call-with-values during
      (lambda results
	(reroot! here)
	(apply values results)))))

(define (reroot! there)

  (define (reroot-loop there)
    (if (not (eq? *here* there))
	(begin (reroot-loop (cdr there))
	       (let ((before (caar there))
		     (after  (cdar there)))
		 (set-car! *here* (cons after before))
		 (set-cdr! *here* there)
		 (set-car! there #f)
		 (set-cdr! there '())
		 (set! *here* there)
		 (before)))))

  (let ((ticks (disable-interrupts)))
    (reroot-loop there)
    (if ticks (enable-interrupts ticks))))

; eof
