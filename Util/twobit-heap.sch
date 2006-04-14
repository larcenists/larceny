; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for building a heap image with all compiler names exposed.
; For SPARC Larceny only.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(load "Util/sparc-unix.sch")
(load-compiler)

(load "Debugger/debug.fasl")
(load "Debugger/inspect-cont.fasl")
(load "Debugger/trace.fasl")
(install-debugger)

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

(dump-interactive-heap "twobit.heap")
(system "./larceny.bin -reorganize-and-dump -heap twobit.heap")
(system "/bin/mv twobit.heap.split twobit.heap")

; eof
