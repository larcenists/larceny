; Ffi/ffi-upper.sch
; Larceny FFI -- upper level of generic C ffi.
;
; $Id$

(define *ffi/libraries* '())		; list of names
(define *ffi/loaded-libraries* '())	; assoc list of ( name . handle )

(define (ffi/libraries . rest)
  (cond ((null? rest)
	 *ffi/libraries*)
	((null? (cdr rest))
	 (set! *ffi/libraries* (car rest))
	 *ffi/libraries*)
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
    (lambda actuals
      (call-with-values
       (lambda ()
	 (ffi/apply tramp args ret actuals))
       (lambda (error? value)
	 (if error?
	     (if (eq? value 'conversion-error)
		 (error "Data conversion error in callout to `" name "'.")
		 (error "Error signalled in callout to `" name "'."))
	     value))))))

; eof
