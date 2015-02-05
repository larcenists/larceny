;;; FIXME: the FFI is flaky/fragile on 64-bit MacOS X machines, so
;;; this defines a current-utc-time that doesn't depend on the FFI
;;; under MacOS X.

(define-library (larceny r7rs primitives patches)

  (export

   jiffies-per-second
   larceny:current-utc-time)

  (import (rnrs base)
          (rnrs lists)
          (primitives r5rs:require
                      current-utc-time
                      system-features
                      current-seconds))

  (begin

   ;; FIXME: add other systems where we can trust the FFI.

   (define *trusted-os* '("SunOS" "Linux"))
   (define *trusted-arch* '("SPARC" "IAssassin" "Standard-C"))

   (define can-use:current-utc-time
     (let* ((alist (system-features))
            (os (assq 'os-name alist))
            (arch (assq 'arch-name alist)))
       (and os
            arch
            (member (cdr os) *trusted-os*)
            (member (cdr arch) *trusted-arch*))))

   (define larceny:current-utc-time
     (if can-use:current-utc-time
         (lambda () (current-utc-time))
         (lambda () (values (current-seconds) 0))))

   (define jiffies-per-second
     (if can-use:current-utc-time
         (lambda () 1000000)
         (lambda () 1)))

   (if can-use:current-utc-time
       (r5rs:require 'time))))

