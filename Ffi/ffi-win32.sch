(define ffi/i386-win32-C-callout-cdecl
  (let ((parent ffi/i386-C-callout-cdecl))

    (define (load-library name)
      (ffi/dlopen name))

    (define (link-procedure library name)
      (ffi/dlsym library name))

    (lambda (selector)
      (case selector
	((load-lib) load-library)
	((link-proc) link-procedure)
	(else (parent selector))))))

