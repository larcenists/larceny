; Ffi/examples.sch
; Larceny FFI -- Low-level FFI examples 
;
; $Id$
;
; See ffi-test.sch for more examples.

; Uncomment one of these to select the right architecture/OS combination.
 
(define FFI-ARCHITECTURE 'sun4-sunos)
;(define FFI-ARCHITECTURE 'sun4-solaris)

; Comment out this is you're loading the FFI system separately.

(load "Ffi/loadffi.sch")
(load-ffi "Ffi/" FFI-ARCHITECTURE)

(define FFI-CALLOUT-ABI
  (case FFI-ARCHITECTURE
    ((sun4-sunos) ffi/SPARC-sunos4-C-callout-stdabi)
    ((sun4-solaris) ffi/SPARC-sunos5-C-callout-stdabi)
    (else ???)))


; System parameters

(define unix:MAXPATHLEN 1024)  ; From <sys/param.h>


; Unix "chdir" (change directory) system call

(define unix:chdir
  (let ((chdir (ffi/foreign-procedure
		FFI-CALLOUT-ABI "chdir" '(pointer) 'void)))
    (lambda (newdir)
      (chdir (ffi/string->asciiz newdir)))))


; Unix "getwd" (get working directory) system call

(define unix:getwd
  (let ((getwd (ffi/foreign-procedure
		FFI-CALLOUT-ABI "getwd" '(pointer) 'void)))
    (lambda ()
      (let ((buf (make-bytevector unix:MAXPATHLEN)))
	(getwd buf)
	(ffi/asciiz->string buf)))))


; Common aliases

(define cd unix:chdir)
(define pwd unix:getwd)

; eof
