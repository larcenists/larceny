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

(load "setup.sch")
(setup 'native
       'string-rep: (cdr (assq 'string-representation (system-features))))
(load-compiler)

(compat:load (param-filename 'debugger "debug.sch"))
(compat:load (param-filename 'debugger "inspect-cont.sch"))
(compat:load (param-filename 'debugger "trace.sch"))
(install-debugger)

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

(dump-interactive-heap "twobit.heap")
(system "./larceny.bin -reorganize-and-dump -heap twobit.heap")
(system "/bin/mv twobit.heap.split twobit.heap")

; eof
