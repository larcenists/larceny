(define ffi/i386-macosx-C-callout-cdecl
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

; Search for a credible version number (one version digit only)

(define (ffi/x86-macosx-libc)
  (let ((basis-name "/usr/lib/libc.dylib"))

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

