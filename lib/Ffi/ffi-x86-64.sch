; Larceny FFI -- x86-64 System V ABI, for Linux.
;
; As on AArch64 (see ffi-arm64.sch, which must be loaded first), there
; is no trampoline code: the trampoline's code bytevector holds the
; address of the foreign function, and larceny_C_ffi_apply in
; src/Rts/Sys/ffi.c makes the call.  Callbacks are not supported.

#!no-fold-case

(define ffi/x86-64-linux-C-callout-stdabi
  (let ((parent ffi/arm64-C-callout-stdabi))
    (lambda (selector)
      (case selector
        ((load-lib) ffi/dlopen)
        ((link-proc) ffi/dlsym)
        (else (parent selector))))))

(define (ffi/x86-64-linux-C-callback-stdabi selector)
  (error "FFI: callbacks are not supported on x86-64."))

; eof
