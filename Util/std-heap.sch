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
; - The FFI and record package internals are not hidden.

(define ($$trace x) #t)			; Called by some files

(load "Util/sysdep-unix.sch")           ; Path name manipulation
(load "Util/nbuild-param-sparc.sch")    ; Parameters for nbuild-files
(define nbuild-parameter
  (make-nbuild-parameter "" #f #f "Larceny" "Larceny"))
(load "Util/nbuild-files.sch")          ; Development system files
(load "Util/load-env.sch")              ; Used to load "modules.list"
(load-environment "Util/modules.list"   ; Load development system
                  'verbose)

(if (and (file-exists? "Util/compile-always.fasl")
	 (file-newer? "Util/compile-always.fasl" "Util/compile-always.sch"))
    (load "Util/compile-always.fasl")
    (load "Util/compile-always.sch"))

;;; Everything loaded after this point will be compiled as it is loaded.

(install-debugger)
(define install-debugger)

;;; Install pretty printer as default printer.

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Load common syntactic abstractions

(load "Auxlib/macros.sch")

;;; Load record package

(load "Auxlib/record.sch")              ; Record package
(load "Experimental/define-record.sch") ; DEFINE-RECORD syntax

;;; Load exception system

(load "Experimental/exception.sch")

;;; Load foreign-function interface

(load "Auxlib/std-ffi.sch")
(load "Auxlib/unix-functions.sch")

;;; Improve some definitions

(define (procedure-documentation-string p)
  (let ((e (procedure-expression p)))
    (if (and (list? e)
             (> (length e) 2)
             (string? (caddr e))
             (not (null? (cdddr e))))
        (caddr e)
        #f)))

;;; Dump the heap

(gctwa)
(dump-interactive-heap "larceny.heap")
(system "./larceny.bin -reorganize-and-dump larceny.heap")
(system "/bin/mv larceny.heap.split larceny.heap")

; eof
