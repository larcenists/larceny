; Ffi/loadffi.sch
; Larceny FFI -- load script
;
; $Id$

(define sun4-sunos4-libraries
  '("/lib/libc.so.1.9"))		; TESTING PURPOSES ONLY!

(define sun4-sunos5-libraries
  '("/lib/libc.so"))			; Should work always (symlink)

(define (load-ffi path architecture/os)

  (define (loadit file)
    (load (string-append path file)))

  (loadit "memory.sch")			; For non-conservative collectors
  (loadit "tramp.sch")			; Trampoline builder (driver)
  (loadit "ffi-sparc.sch")		; Trampoline builder (code generator)
  (loadit "ffi-lower.sch")		; FFI/apply and all that
  (loadit "ffi-upper.sch")		; High-level load/link code
  (loadit "ffi-util.sch")		; Data conversion

  (case architecture/os
    ((sun4-sunos4)
     (loadit "ffi-sunos4.sch")
     (ffi/libraries sun4-sunos4-libraries))
    ((sun4-sunos5)
     (loadit "ffi-sunos5.sch")
     (ffi/libraries sun4-sunos5-libraries))
    (else
     ???))

  "FFI loaded")

; eof
