; Copyright 2004 Lars T Hansen
;
; Generic mostly-portable scripts for cleaning up generated files.
;
; This file can be loaded without loading anything else, but it
; is also loaded by the build system.
;
; CLEAN-ALL      - remove all generated files and directories
; CLEAN-OBJ      - remove executables, libraries, and object files
; CLEAN-FASL     - remove all .FASL files
; CLEAN-LAP      - remove all .LAP files
; CLEAN-LOP      - remove all .LOP files

(define *cleanup-verbose* #f)		; set to #t to print dirs and commands

(define rm-command)
(define rmdir-command)
(define pathsep)

(case (cdr (assq 'os-name (system-features)))
  ((win32) 
   (set! rm-command "del")
   (set! rmdir-command "rmdir")
   (set! pathsep "\\"))
  (else
   (set! rm-command "rm -f")
   (set! rmdir-command "rmdir")
   (set! pathsep "/")))

(define *schemedirs*
  '(("Lib")
    ("Lib" "Common")
    ("Lib" "Standard-C")
    ("Interpreter")
    ("Repl")
    ("Auxlib")
    ("Twobit")
    ("Compat" "Larceny")
    ("Asm")
    ("Asm" "Common")
    ("Asm" "Standard-C")
    ("Asm" "Intel")
    ("Util")
    ("Debugger")
    ("Experimental")
    ("Ffi")
    ("Testsuite")
    ("Testsuite" "Boot")
    ("Testsuite" "Compiler")
    ("Testsuite" "FFI")
    ("Testsuite" "GC")
    ("Testsuite" "Jaffer")
    ("Testsuite" "Lib")
    ("Testsuite" "Misc")
    ("Testsuite" "Sparc")
    ("Testsuite" "Stress")))

(define *rtsdirs*
  '(("Rts") 
    ("Rts" "Build")
    ("Rts" "Sys")
    ("Rts" "Sparc")
    ("Rts" "Standard-C")
    ("Rts" "Intel")))

(define *libdirs*
  '((".")
    ("Rts")))

(define (cleanup-file pattern)
  (cleanup-exec (string-append rm-command " " pattern)))

(define (cleanup-files pattern dirs)
  (for-each (lambda (dir)
	      (with-current-directory (construct-path dir)
	       (lambda ()
		 (cleanup-file pattern))))
	    dirs))

(define (cleanup-directory dirspec)
  (let ((dir (construct-path dirspec)))
    (if (file-exists? dir)
	(begin
	  (with-current-directory dir
            (lambda ()
	      (cleanup-file "*.*")))
	  (cleanup-exec (string-append rmdir-command " " dir))))))

(define (cleanup-exec cmd)
  (if *cleanup-verbose*
      (begin
	(display (current-directory))
	(display ": ")
	(display cmd)
	(newline)))
  (system cmd))

(define (construct-path dir)
  (let loop ((p (car dir)) (dir (cdr dir)))
    (if (null? dir)
	p
	(loop (string-append p pathsep (car dir)) (cdr dir)))))

(define (with-current-directory dir thunk)
  (let ((cdir #f))
    (dynamic-wind
	(lambda ()
	  (set! cdir (current-directory))
	  (current-directory dir))
	thunk
	(lambda ()
	  (set! dir (current-directory))
	  (current-directory cdir)))))

(define (clean-all)
  (clean-obj)
  (clean-lap)
  (clean-lop)
  (clean-fasl)
  (for-each cleanup-file
	    '("core" 
	      "petit" "petit.exe" "petit.c" "petit.o" "petit.obj" 
	      "twobit" "twobit.exe" "twobit.app" "twobit.c" "twobit.o" "twobit.obj"
	      "*.heap" "HEAPDATA.*" "*.fasl"))
  (cleanup-files "arithmetic.c" '(("Rts" "Standard-C")))
  (cleanup-directory '("Rts" "Build"))
  (unspecified))

(define (clean-obj)
  (cleanup-files "*.o" *schemedirs*)
  (cleanup-files "*.o" *rtsdirs*)
  (cleanup-files "*.obj" *schemedirs*)
  (cleanup-files "*.obj" *rtsdirs*)
  (cleanup-files "*.lib" *libdirs*)
  (cleanup-files "*.a" *libdirs*)
  (cleanup-files "*.dylib" *libdirs*)
  (cleanup-files "*.so" *libdirs*)
  (cleanup-files "*.c" *schemedirs*)
  (cleanup-files "*.asm" *schemedirs*)
  (unspecified))

(define (clean-lap)
  (cleanup-files "*.lap" *schemedirs*)
  (cleanup-files "*.lap" *rtsdirs*)
  (unspecified))

(define (clean-lop)
  (cleanup-files "*.lop" *schemedirs*)
  (cleanup-files "*.lop" *rtsdirs*)
  (unspecified))

(define (clean-fasl)
  (cleanup-files "*.fasl" *schemedirs*)
  (cleanup-files "*.fasl" *rtsdirs*)
  (unspecified))

