;;; FIXME: the FFI is flaky/fragile on 64-bit MacOS X machines, so
;;; this defines a current-utc-time that doesn't depend on the FFI
;;; under MacOS X, except on Apple Silicon (arm64), where the FFI makes
;;; the call itself (no trampoline code).

(define-library (larceny r7rs primitives patches)

  (export

   jiffies-per-second
   larceny:current-utc-time
   larceny:monotonic-nanoseconds)

  (import (rnrs base)
          (rnrs lists)
          (primitives r5rs:require
                      current-utc-time
                      system-features
                      current-seconds
                      larceny:monotonic-nanoseconds))

  (begin

   ;; FIXME: add other systems where we can trust the FFI.

   (define *trusted-os* '("SunOS" "Linux"))
   (define *trusted-arch* '("SPARC" "IAssassin" "Standard-C" "X86-64"))

   ;; The only systems with 64-bit words on MacOS are arm64 ones:
   ;; native (ARM) and Petit (Standard-C).  See lib/Ffi/ffi-load.sch.

   (define can-use:current-utc-time
     (let* ((alist (system-features))
            (os (assq 'os-name alist))
            (arch (assq 'arch-name alist))
            (bits (assq 'arch-word-size alist)))
       (and os
            arch
            (or (and (member (cdr os) *trusted-os*)
                     (member (cdr arch) *trusted-arch*))
                (and (string=? (cdr os) "MacOS X")
                     bits
                     (= (cdr bits) 64)
                     (member (cdr arch) '("ARM" "Standard-C")))))))

   ;; The time library (and the FFI) is loaded when current-utc-time is
   ;; first called, not when this library is: loading it takes about
   ;; 100 ms.

   (define time-library-loaded? #f)

   (define larceny:current-utc-time
     (if can-use:current-utc-time
         (lambda ()
           (if (not time-library-loaded?)
               (begin (r5rs:require 'time)
                      (set! time-library-loaded? #t)))
           (current-utc-time))
         (lambda () (values (current-seconds) 0))))

   ;; Jiffies are nanoseconds of the monotonic clock where that is a
   ;; fixnum (64-bit Unix systems; see primitive_monotonic_ns in
   ;; src/Rts/Sys/primitive.c), else microseconds or seconds of the
   ;; time of day.

   (define jiffies-per-second
     (cond ((larceny:monotonic-nanoseconds)
            (lambda () 1000000000))
           (can-use:current-utc-time
            (lambda () 1000000))
           (else
            (lambda () 1))))))
