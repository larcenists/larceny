; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for building the full heap image on SPARC.
;
; Before you use this script, you must compile the development environment
; and the debugger.  The easiest way to do that is to run 'build' and then
; evaluate
;   (make-development-environment)
;
; BUGS:
; - The FFI internals are not hidden.

(define ($$trace x) #t)			; Called by some files.

(load "Util/sysdep-unix.sch")
(load "Util/nbuild-param-sparc.sch")	         ; Parameters for nbuild-files.
(load "Util/nbuild-files.sch")		         ; Development system files.
(load "Util/load-env.sch")		         ; Used to load modules.list.
(load-environment "Util/modules.list" 'verbose)	 ; Load development system.

(if (and (file-exists? "Util/compile-always.fasl")
	 (file-newer? "Util/compile-always.fasl" "Util/compile-always.sch"))
    (load "Util/compile-always.fasl")
    (load "Util/compile-always.sch"))

; Everything will be compiled from now on.

(repl-printer
 (lambda (x)
   (if (not (eq? x (unspecified)))
       (pretty-print x))))

; Common syntactic abstractions
(load "Auxlib/macros.sch")

; Records

(load "Experimental/record.sch")        ; Record package
(load "Experimental/define-record.sch") ; DEFINE-RECORD syntax

; Foreign-function interface

(load "Auxlib/std-ffi.sch")
(load "Auxlib/unix-functions.sch")

; Improved definitions 

(define apropos
  (let ((apropos apropos))
    (lambda (x)
      (sort (apropos x) 
	    (lambda (a b)
	      (string<? (symbol->string a) (symbol->string b)))))))

(dump-interactive-heap "larceny.heap")
(system "./larceny.bin -reorganize-and-dump larceny.heap")
(system "/bin/mv larceny.heap.split larceny.heap")

; eof
