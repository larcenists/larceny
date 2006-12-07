(define ffi/i386-linux-C-callout-cdecl
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

(define ffi/i386-linux-C-callback-cdecl
  (let ((parent ffi/i386-linux-C-callout-cdecl))
    (define (callback-addr)
      ((let ((a (ffi/getaddr 'convert-and-call)))
         (if a a (error "callback-addr (linux x86): failed.")))))
    
    (lambda (selector)
      (case selector
        ((callback-addr)  callback-addr)
        (else (parent selector))))))

; Search for a credible version number (one version digit only)

(define (ffi/x86-linux-libc)
  (let ((basis-name "/lib/libc.so"))

    (define (loop-maj maj)
      (if (= maj 10)
	  #f
	  (let ((fn (string-append basis-name
				   "."
				   (number->string maj))))
	    (if (file-exists? fn)
		fn
		(loop-maj (+ maj 1))))))

    (if (file-exists? basis-name)
	basis-name
	(loop-maj 1))))

; eof

