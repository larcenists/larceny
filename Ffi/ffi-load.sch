; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- simple load script.

(define (load-ffi path)

  (define architecture
    (let* ((f   (system-features))
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
            ((and (string=? os "Linux")
                  (zero?
                   (system "test \"`uname -m | grep 'i.86'`x\" != \"x\"")))
             'i386-linux)
            (else
             (error "FFI: unsupported operating system " os)))))

  (define (loadit file)
    (load (string-append path file)))

  (loadit "memory.sch")                 ; For non-conservative collectors
  (loadit "tramp.sch")                  ; Trampoline builder (driver)
  (loadit "ffi-lower.sch")              ; FFI/apply and all that
  (loadit "ffi-upper.sch")              ; High-level load/link code
  (loadit "ffi-util.sch")               ; Data conversion

  (case architecture
    ((sun4-sunos4)
     (loadit "ffi-sparc.sch")
     (loadit "ffi-sunos4.sch")
     (ffi/libraries (list (ffi/sun4-sunos4-libc)))
     (values architecture
             ffi/SPARC-sunos4-C-callout-stdabi
             ffi/SPARC-sunos4-C-callback-stdabi))
    ((sun4-sunos5)
     (loadit "ffi-sparc.sch")
     (loadit "ffi-sunos5.sch")
     (ffi/libraries (list (ffi/sun4-sunos5-libc)))
     (values architecture
             ffi/SPARC-sunos5-C-callout-stdabi
             ffi/SPARC-sunos5-C-callback-stdabi))
    ((i386-win32)
     (loadit "ffi-i386.sch")
     (loadit "ffi-win32.sch")
     (values architecture
             ffi/i386-win32-C-callout-cdecl
             #f))
    ((i386-linux)
     (loadit "ffi-i386.sch")
     (loadit "ffi-linux-x86.sch")
     (ffi/libraries (list (ffi/x86-linux-libc)))
     (values architecture
             ffi/i386-linux-C-callout-cdecl
             #f))
    (else
     (error "Unknown FFI architecture " *ffi-architecture*))))

; eof
