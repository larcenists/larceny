; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- ABI objects for sun4-sunos4.

#!no-fold-case

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


; On SunOS 4, libc.so also has a version number, and at least on 
; the systems at CCS, there is no symlink from /lib/libc.so to the
; current version (SunOS 5, in contrast, seems to have this).  So
; we must search.

(define (ffi/sun4-sunos4-libc)
  (let ((basis-name "/lib/libc.so"))

    (define (loop-maj maj)
      (if (= maj 10) 
	  #f
	  (loop-min maj 0)))

    (define (loop-min maj min)
      (if (= min 20)
	  (loop-maj (+ maj 1))
	  (let ((fn (string-append basis-name
				   "."
				   (number->string maj)
				   "."
				   (number->string min))))
	    (if (file-exists? fn)
		fn
		(loop-min maj (+ min 1))))))

    (if (file-exists? basis-name)
	basis-name
	(loop-maj 1))))

; eof
