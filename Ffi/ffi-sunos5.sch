; OS-specific ABIs.

(define ffi/SPARC-sunos5-C-callout-stdabi
  (let ((parent ffi/SPARC-C-callout-stdabi))

    (define (load-library name)
      (ffi/dlopen name))

    (define (link-procedure library name)
      (ffi/dlsym library name))

    (lambda (selector)
      (case selector
	((load-lib) load-library)
	((link-proc) link-procedure)
	(else (parent selector))))))

(define ffi/SPARC-sunos5-C-callback-stdabi
  (let ((parent ffi/SPARC-C-callback-stdabi))

    (define (callback-addr)
      (let ((a (ffi/dlsym 0 "larceny_C_ffi_convert_and_call")))
	(if a
	    a
	    (error "callback-addr (sunos5): failed."))))

    (lambda (selector)
      (case selector
	((callback-addr)  callback-addr)
	(else (parent selector))))))


