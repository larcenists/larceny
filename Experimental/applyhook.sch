; Experimental/applyhook.sch
; Larceny library -- application hooks: generalized procedures
;
; $Id$
;
; The concept comes from MIT Scheme (at least in my universe), as do the
; names.  An "apply hook" is an object that can be applied like a procedure.
; It contains a procedure and an "extra" object.  When an apply hook is 
; applied, the procedure stored in the hook is called with the arguments 
; given to the hook.
;
; MIT Scheme also has the notion of 'entities', which I have not yet
; implemented, but they are similar.
;
; See also the file applyhook0.mal for low-level support code.

; This belongs in Lib/typetags.sch

(define sys$tag.applyhook-typetag 0)

(define (make-apply-hook proc object)
  (let ((a (procedure-arity proc)))
    (let ((h (if (and (integer? a) (exact? a))
		 (case (procedure-arity proc)
		   ((1) (make-apply-hook-1 proc object))
		   ((2) (make-apply-hook-2 proc object))
		   ((3) (make-apply-hook-3 proc object))
		   ((4) (make-apply-hook-4 proc object))
		   ((5) (make-apply-hook-5 proc object))
		   (else (make-apply-hook-n proc object)))
		 (make-apply-hook-n proc object))))
      (typetag-set! h sys$tag.applyhook-typetag)
      h)))

(define (apply-hook? obj)
  (and (procedure? obj)
       (= (typetag obj) sys$tag.applyhook-typetag)))

(define (apply-hook-procedure obj)
  (if (apply-hook? obj)
      (procedure-ref obj *apply-hook-proc-offset*)
      (error "apply-hook-procedure: " obj " is not an apply hook.")))

(define (set-apply-hook-procedure! obj proc)
  (if (apply-hook? obj)
      (procedure-set! obj *apply-hook-proc-offset* proc)
      (error "set-apply-hook-procedure!: " obj " is not an apply hook.")))

(define (apply-hook-extra obj)
  (if (apply-hook? obj)
      (procedure-ref obj *apply-hook-obj-offset*)
      (error "apply-hook-extra: " obj " is not an apply hook.")))

(define (set-apply-hook-extra! obj val)
  (if (apply-hook? obj)
      (procedure-set! obj *apply-hook-obj-offset* val)
      (error "set-apply-hook-extra!: " obj " is not an apply hook.")))

; eof 
