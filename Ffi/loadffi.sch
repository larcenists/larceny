; Ffi/loadffi.sch
; Larceny FFI -- load script
;
; $Id$

(define sun4-sunos-libraries
  '("/usr/lib/libc.so.1.9"))		; TESTING PURPOSES ONLY!

(define sun4-solaris-libraries
  '("/usr/lib/libc.so"))		; Should work always (symlink)

(define (load-ffi path architecture/os)

  (load "Ffi/memory.sch")               ; for non-conservative collectors
  (load "Ffi/ffi-sparc.sch")		; trampoline builder (code generator)
  (load "Ffi/tramp.sch")		; trampoline builder (driver)
  (load "Ffi/ffi-lower.sch")		; ffi/apply and all that
  (load "Ffi/ffi-upper.sch")		; high-level load/link code
  (load "Ffi/ffi-util.sch")		; data conversion

  (case architecture/os
    ((sun4-sunos)
     (load "Ffi/ffi-sunos4.sch")	; SunOS 4.x.x
     (ffi/libraries sun4-sunos-libraries))
    ((sun4-solaris)
     (load "Ffi/ffi-sunos5.sch")
     (ffi/libraries sun4-solaris-libraries))
    (else
     ???))

  "FFI loaded")

; eof
