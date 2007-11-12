; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- simple load script.

(define (load-ffi)

  (define architecture
    (let* ((f   (system-features))
	   (ar  (cdr (assq 'arch-name f)))
	   (os  (cdr (assq 'os-name f)))
	   (maj (cdr (assq 'os-major-version f))))
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
    (else
     (error "Unknown FFI architecture " *ffi-architecture*))))

; eof
