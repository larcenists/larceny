; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for building the full heap image on SPARC.
;
; BUGS:
; - The FFI internals are not hidden.

(load "Util/nbuild-param-sparc.sch")	         ; Parameters for nbuild-files.
(load "Util/nbuild-files.sch")		         ; Development system files.
(load "Util/load-env.sch")		         ; Used to load modules.list.
(load-environment "Util/modules.list" 'verbose)	 ; Load development system.

(if (and (file-exists? "Util/compile-always.fasl")
	 (file-newer? "Util/compile-always.fasl" "Util/compile-always.sch"))
    (load "Util/compile-always.fasl")
    (load "Util/compile-always.sch"))

; Everything will be compiled from now on.

(load "Auxlib/std-ffi.sch")
(load "Auxlib/unix-functions.sch")

(define apropos
  (let ((apropos apropos))
    (lambda (x)
      (sort (apropos x) 
	    (lambda (a b)
	      (string<? (symbol->string a) (symbol->string b)))))))

(herald (string-append "\nStandard heap image dumped on "
		       (unix:current-timestamp)
		       ".\nUsing compiler for evaluation."))
(repl-display-procedure 
 (lambda (x)
   (if (not (eq? x (unspecified)))
       (pretty-print x))))

(dump-interactive-heap "std.heap")
(system "larceny -reorganize-and-dump std.heap")
(system "mv std.heap.split std.heap")

; eof
