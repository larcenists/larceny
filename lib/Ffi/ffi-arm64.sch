; Larceny FFI -- AArch64 (64-bit ARM) ABI, for MacOS.
;
; There is no trampoline code on AArch64: the trampoline's code
; bytevector holds the address of the foreign function (8 bytes, native
; byte order), and larceny_C_ffi_apply in src/Rts/Sys/ffi.c assigns
; the arguments to registers and the stack from the argument
; descriptor.  Callbacks are not supported.

#!no-fold-case

(define ffi/arm64-C-callout-stdabi)
(define ffi/arm64-macosx-C-callout-stdabi)
(define ffi/arm64-macosx-C-callback-stdabi)

(let ()

  (define (alloc-trampoline)
    (let ((tr (make-vector *trampoline-basis-size*)))
      (tr-init-common tr)
      tr))

  (define (address->bytevector addr)
    (let ((bv (make-bytevector 8)))
      (bytevector-u64-native-set! bv 0 addr)
      bv))

  (define (ignore tr) #t)

  (define (callout-done tr)
    (set-tr-ilist! tr (list (address->bytevector (tr-fptr tr)))))

  (define (callout-abi selector)
    (case selector
      ((alloc) alloc-trampoline)
      ((arg-word arg-word2 arg-ieee32 arg-ieee64) ignore)
      ((ret-word ret-word2 ret-ieee32 ret-ieee64 ret-void) ignore)
      ((done change-fptr) callout-done)
      ((done-pasteup) ignore)
      (else
       (error "ffi/arm64-C-callout-stdabi: unknown selector " selector))))

  (define (callback-abi selector)
    (error "FFI: callbacks are not supported on AArch64."))

  (set! ffi/arm64-C-callout-stdabi callout-abi)

  (set! ffi/arm64-macosx-C-callout-stdabi
        (lambda (selector)
          (case selector
            ((load-lib) ffi/dlopen)
            ((link-proc) ffi/dlsym)
            (else (callout-abi selector)))))

  (set! ffi/arm64-macosx-C-callback-stdabi callback-abi))

; The C library, which on MacOS is part of libSystem.

(define (ffi/arm64-macosx-libc)
  "/usr/lib/libSystem.B.dylib")

; eof
