; Copyright 2006 Felix S Klock
;
; $Id: twobit-heap.sch 2916 2006-04-27 21:25:00Z tov $
;
; Load script for building a heap image with all compiler names exposed.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(load "setup.sch")
(setup 'sassy)
(load-compiler)

(compat:load (param-filename 'debugger "debug.fasl"))
(compat:load (param-filename 'debugger "inspect-cont.fasl"))
(compat:load (param-filename 'debugger "trace.fasl"))
(install-debugger)

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

(dump-interactive-heap "twobit.heap")
(cond
 ((equal? (cdr (assq 'os-name (system-features)))
	  "Win32")
  (system "larceny.bin -reorganize-and-dump -heap twobit.heap")
  (system "move twobit.heap.split twobit.heap"))
 (else
  (system "./larceny.bin -reorganize-and-dump -heap twobit.heap")
  (system "/bin/mv twobit.heap.split twobit.heap")))

; eof
