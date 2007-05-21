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

(define ffi/i386-win32-C-callback-cdecl
  (let ((parent ffi/i386-C-callback-cdecl))
    (define (callback-addr)
      (let ((a (ffi/getaddr 'convert-and-call)))
        (if a a (error "callback-addr (win32 x86): failed."))))
    
    (lambda (selector)
      (case selector
        ((callback-addr)  callback-addr)
        (else (parent selector))))))

(define ffi/i386-win32-C-callout-stdcall
  (let ((parent ffi/i386-C-callout-stdcall))

    (define (load-library name)
      (ffi/dlopen name))

    (define (link-procedure library name)
      (ffi/dlsym library name))

    (lambda (selector)
      (case selector
	((load-lib) load-library)
	((link-proc) link-procedure)
	(else (parent selector))))))

(define (ffi/x86-win32-libs)
  (list "kernel32" "msvcrt"))
