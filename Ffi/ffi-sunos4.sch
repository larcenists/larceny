; Ffi/ffi-sunos4.sch
; Larceny FFI -- ABI objects for sun4-sunos4
;
; $Id$

(define ffi/SPARC-sunos4-C-callout-stdabi
  (let ((parent ffi/SPARC-C-callout-stdabi))

    (define (load-library name)
      (ffi/dlopen name))

    (define (link-procedure library name)
      (ffi/dlsym library (string-append "_" name)))

    (lambda (selector)
      (case selector
	((load-lib) load-library)
	((link-proc) link-procedure)
	(else (parent selector))))))


(define ffi/SPARC-sunos4-C-callback-stdabi
  (let ((parent ffi/SPARC-C-callback-stdabi))

    (define (callback-addr)
      (let ((a (ffi/getaddr 'convert-and-call)))
	(if a
	    a
	    (error "callback-addr (sunos4): failed."))))

    (lambda (selector)
      (case selector
	((callback-addr)  callback-addr)
	(else (parent selector))))))

; eof
