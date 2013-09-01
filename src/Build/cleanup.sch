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

(define rm-command #f)
(define rmdir-command #f)
(define pathsep #f)
(define exe-suffix #f)

(let ((os (cdr (assq 'os-name (system-features)))))
  (cond ((equal? os "Win32")
	 (set! exe-suffix ".bin.exe")
	 (set! rm-command "del")
	 (set! rmdir-command "rmdir")
	 (set! pathsep "\\"))
	(else
	 (set! exe-suffix ".bin")
	 (set! rm-command "rm -f")
	 (set! rmdir-command "rmdir")
	 (set! pathsep "/"))))

(define *schemedirs*
  '(
    ("src" "Asm")
    ("src" "Asm" "Common")
    ("src" "Asm" "Standard-C")
    ("src" "Asm" "Intel")
    ("src" "Asm" "Sparc")
    ("src" "Asm" "IL")
    ("src" "Asm" "IAssassin")

    ("src" "Lib")
    ("src" "Lib" "Arch" "Fence")
    ("src" "Lib" "Arch" "IAssassin")
    ("src" "Lib" "Arch" "IL")
    ("src" "Lib" "Arch" "Sparc")
    ("src" "Lib" "Arch" "Standard-C")
    ("src" "Lib" "Common")
    ("src" "Lib" "Common" "SimpleMacros")
    ("src" "Lib" "Interpreter")
    ("src" "Lib" "Repl")
    ("src" "Lib" "Sassy")
    ("Compiler")
    ("Compat" "Larceny")
    ("Compat" "MzScheme")
    ("lib" "SRFI")
    ("lib" "Standard")
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

; There are exceptions to every rule...
(define *exceptions*
  '(("Experimental" "*.c")
    ("Testsuite" "FFI" "*.c")))

(define (cleanup-file pattern)
  (cleanup-exec (string-append rm-command " " pattern)))

(define (cleanup-files pattern dirs)
  (for-each (lambda (dir)
	      (if (not (member (append dir (list pattern)) *exceptions*))
		  (with-current-directory (construct-path dir)
		    (lambda ()
		      (cleanup-file pattern)))))
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
	    `("core" 
	      ,(string-append "petit" exe-suffix)
              "petit.c" "petit.o" "petit.obj" 
	      ,(string-append "twobit" exe-suffix)
              "twobit.c" "twobit.o" "twobit.obj"
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
  (cleanup-files "*.dll" *libdirs*)
  (cleanup-files "*.so" *libdirs*)
  (cleanup-files "*.c" *schemedirs*)
  (cleanup-files "*.asm" *schemedirs*)
  (cleanup-files "*.code-il" *schemedirs*)
  (cleanup-files "*.il" *schemedirs*)
  (cleanup-files "*.manifest" *schemedirs*)
  (cleanup-files "ecodes.sch" '(("Lib" "Common")))
  (cleanup-files "globals.sch" '(("Lib" "Common")))
  (cleanup-files "*.so" '(("Testsuite" "FFI")))
  (cleanup-files "*.dll" '(("Testsuite" "FFI")))
  (cleanup-files "*.pdb" '(("Testsuite" "FFI")))
  (cleanup-files "*.pdb" '(("Rts" "DotNet")))
  (cleanup-files "*.ilk" '(("Testsuite" "FFI")))
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

(define (clean-runtime)
  (cleanup-files "*.o" *rtsdirs*)
  (cleanup-files "*.obj" *rtsdirs*)
  (cleanup-files "*.a" *rtsdirs*)
  (cleanup-files "*.lib" *rtsdirs*))

