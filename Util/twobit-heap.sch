; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for building a heap image with all compiler names exposed.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build' and then 
; evaluate
;   (make-development-environment)

(load "Util/load-twobit-sparc.sch")

(load "Auxlib/io.fasl")
(load "Auxlib/string.fasl")
(load "Experimental/apropos.fasl")

(load "Debugger/debug.fasl")
(load "Debugger/inspect-cont.fasl")
(load "Debugger/trace.fasl")
(install-debugger)

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

(dump-interactive-heap "twobit.heap")
(system "./larceny.bin -reorganize-and-dump twobit.heap")
(system "/bin/mv twobit.heap.split twobit.heap")

; eof
