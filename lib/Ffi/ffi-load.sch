; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- simple load script.

#!no-fold-case

(define (load-ffi)

  (define architecture
    (let* ((f   (system-features))
	   (ar  (cdr (assq 'arch-name f)))
	   (os  (cdr (assq 'os-name f)))
	   (maj (cdr (assq 'os-major-version f)))
	   (bits (cdr (assq 'arch-word-size f))))
      (cond ((string=? os "SunOS")
	     (case maj
	       ((4) 'sun4-sunos4)
	       ((5) 'sun4-sunos5)
	       (else 
		(error "FFI: unsupported SunOS version " maj))))
	    ((string=? os "Win32")
	     'i386-win32)
	    ((and (string=? os "MacOS X") (string=? ar "IAssassin"))
	     'i386-macosx)
	    ;; The only MacOS systems with 64-bit words are arm64 ones.
	    ((and (string=? os "MacOS X")
		  (= bits 64)
		  (or (string=? ar "ARM") (string=? ar "Standard-C")))
	     'arm64-macosx)
	    ((and (string=? os "Linux")
		  (string=? ar "ARM"))
	     'arm-linux)
	    ((and (string=? os "Linux")
		  (zero?
		   (system "test \"`uname -m | grep 'i.86'`x\" != \"x\"")))
	     'i386-linux)
            ((and (string=? os "Linux")
                  (zero?
                   (system "test \"`uname -m | grep 'x86_64'`x\" != \"x\"")))
             'i386-linux)
	    (else
	     (error "FFI: unsupported operating system " os)))))

  (require "memory")			; For non-conservative collectors
  (require "tramp")			; Trampoline builder (driver)
  (require "ffi-lower")		; FFI/apply and all that
  (require "ffi-upper")		; High-level load/link code
  (require "ffi-util")		; Data conversion

  (case architecture
    ((sun4-sunos4)
     (require "ffi-sparc")
     (require "ffi-sunos4")
     (ffi/libraries (list (ffi/sun4-sunos4-libc)))
     (values architecture
	     ffi/SPARC-sunos4-C-callout-stdabi
	     ffi/SPARC-sunos4-C-callback-stdabi
             #f
             #f))
    ((sun4-sunos5)
     (require "ffi-sparc")
     (require "ffi-sunos5")
     (ffi/libraries (list (ffi/sun4-sunos5-libc)))
     (values architecture
	     ffi/SPARC-sunos5-C-callout-stdabi
	     ffi/SPARC-sunos5-C-callback-stdabi
             #f
             #f))
    ((i386-macosx)
     (require "ffi-i386")
     (require "ffi-macosx")
     (ffi/libraries (list (ffi/x86-macosx-libc)))
     (values architecture
             ffi/i386-macosx-C-callout-cdecl
             ffi/i386-macosx-C-callback-cdecl
             #f
             #f))
    ((i386-win32)
     (require "ffi-i386")
     (require "ffi-win32")
     (ffi/libraries (ffi/x86-win32-libs))
     (values architecture
	     ffi/i386-win32-C-callout-cdecl
	     ffi/i386-win32-C-callback-cdecl
             ffi/i386-win32-C-callout-stdcall
             #f))
    ((i386-linux)
     (require "ffi-i386")
     (require "ffi-linux-x86")
     ;(ffi/libraries (list (ffi/x86-linux-libc)))
     (values architecture
	     ffi/i386-linux-C-callout-cdecl
	     ffi/i386-linux-C-callback-cdecl
             #f
             #f))
    ((arm64-macosx)
     (require "ffi-arm64")
     (ffi/libraries (list (ffi/arm64-macosx-libc)))
     (values architecture
             ffi/arm64-macosx-C-callout-stdabi
             ffi/arm64-macosx-C-callback-stdabi
             #f
             #f))
    ((arm-linux)
     (error "ffi-load.sch: arm-linux not yet supported"))
    (else
     (error "Unknown FFI architecture " *ffi-architecture*))))

; eof
