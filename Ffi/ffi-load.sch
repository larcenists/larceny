; Ffi/ffi-load.sch
; Larceny FFI -- simple load script
;
; $Id$

(define (load-ffi path)

  (define architecture
    (let* ((f   (system-features))
	   (os  (cdr (assq 'operating-system-name f)))
	   (maj (cdr (assq 'os-major-version f))))
      (cond ((string=? os "SunOS")
	     (case maj
	       ((4) 'sun4-sunos4)
	       ((5) 'sun4-sunos5)
	       (else 
		(error "FFI: unsupported SunOS version " maj))))
	    (else
	     (error "FFI: unsupported operating system " os)))))

  (define (loadit file)
    (load (string-append path file)))

  (loadit "memory.sch")			; For non-conservative collectors
  (loadit "tramp.sch")			; Trampoline builder (driver)
  (loadit "ffi-sparc.sch")		; Trampoline builder (code generator)
  (loadit "ffi-lower.sch")		; FFI/apply and all that
  (loadit "ffi-upper.sch")		; High-level load/link code
  (loadit "ffi-util.sch")		; Data conversion

  (case architecture
    ((sun4-sunos4)
     (loadit "ffi-sunos4.sch")
     (ffi/libraries (list (ffi/sun4-sunos4-libc)))
     (values architecture
	     ffi/SPARC-sunos4-C-callout-stdabi
	     ffi/SPARC-sunos4-C-callback-stdabi))
    ((sun4-sunos5)
     (loadit "ffi-sunos5.sch")
     (ffi/libraries (list (ffi/sun4-sunos5-libc)))
     (values architecture
	     ffi/SPARC-sunos5-C-callout-stdabi
	     ffi/SPARC-sunos5-C-callback-stdabi))
    (else
     (error "Unknown FFI architecture " *ffi-architecture*))))

; eof
