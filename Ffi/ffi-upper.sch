; Ffi/ffi-upper.sch
; Larceny FFI -- upper level of generic C ffi.
;
; $Id: ffi-upper.sch,v 1.1.1.1 1998/11/19 21:52:28 lth Exp $
;
; FIXME: *ffi/linked-procedures* needs to hold the procedures weakly.

(define *ffi/libraries* '())		; list of names
(define *ffi/loaded-libraries* '())	; list of ( name . handle )
(define *ffi/linked-procedures* '())    ; list of ( name abi trampoline libs )

(define (ffi/libraries . rest)
  (cond ((null? rest)
	 (list-copy *ffi/libraries*))
	((null? (cdr rest))
	 (set! *ffi/libraries* (list-copy (car rest)))
	 (unspecified))
	(else
	 (error "ffi/libraries: Too many arguments."))))

(define (ffi/load-libraries abi)

  (define (open-library name)
    (let ((probe ((abi 'load-lib) name)))
      (if probe
	  probe
	  (begin (format #t "ffi/load-libraries: ~a can't be opened." name)
		 #f))))

  (define (merge-libraries libs result)
    (cond ((null? libs)
	   (reverse result))
	  ((assoc (car libs) *ffi/loaded-libraries*)
	   =>
	   (lambda (probe)
	     (merge-libraries (cdr libs)
			      (cons probe result))))
	  ((open-library (car libs))
	   =>
	   (lambda (handle)
	     (merge-libraries (cdr libs)
			      (cons (cons (car libs) handle) result))))
	  (else
	   (merge-libraries (cdr libs) result))))

  (set! *ffi/loaded-libraries* (merge-libraries (ffi/libraries) '()))
  *ffi/loaded-libraries*)

(define (ffi/link-procedure abi name)
  (let loop ((libs (ffi/load-libraries abi)))
    (cond ((null? libs)
	   (error "ffi/find-procedure: procedure " name " can't be found."))
	  (((abi 'link-proc) (cdar libs) name))
	  (else
	   (loop (cdr libs))))))

(define (ffi/foreign-procedure abi name args ret)
  (let* ((addr  (ffi/link-procedure abi name))
	 (tramp (ffi/make-callout abi addr args ret))
	 (args  (ffi/convert-arg-descriptor abi args))
	 (ret   (ffi/convert-ret-descriptor abi ret)))
    (set! *ffi/linked-procedures*
	  (cons (list name abi tramp *ffi/libraries*) *ffi/linked-procedures*))
    (lambda actuals
      (call-with-values
       (lambda ()
	 (ffi/apply tramp args ret actuals))
       (lambda (error? value)
	 (if error?
	     (if (eq? value 'conversion-error)
		 (error "Data conversion error in callout to \"" name "\".")
		 (error "Error signalled in callout to \"" name "\"."))
	     value))))))

(define (ffi/initialize-after-load-world)
;  (display "; Reloading foreign functions")
;  (newline)
  (set! *ffi/loaded-libraries* '())	; Force files to be re-loaded
  (ffi/relink-all-procedures))		; Update procedures with new addresses

(define (ffi/relink-all-procedures)
  (call-without-interrupts
    (lambda ()
      (for-each (lambda (x)
		  (let ((name  (car x))
			(abi   (cadr x))
			(tramp (caddr x))
			(libs  (cadddr x))
			(old-libs (ffi/libraries)))
		    (ffi/libraries libs)
		    (let ((new-addr (ffi/link-procedure abi name)))
		      (ffi/libraries old-libs)
		      (ffi/set-callout-address! abi tramp new-addr))))
		*ffi/linked-procedures*))))

(add-init-procedure! ffi/initialize-after-load-world)

; eof
