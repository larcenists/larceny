; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for building a heap image with all compiler names exposed.

(load "Util/load-twobit-sparc.sch")

(load "Auxlib/io.fasl")
(load "Experimental/apropos.fasl")

(load "Auxlib/optimize-level.sch")
(load "Auxlib/std-ffi.sch")
(load "Auxlib/ffi-functions.sch")

(define pp pretty-print)

(define apropos
  (let ((apropos apropos))
    (lambda (x)
      (sort (apropos x) 
	    (lambda (a b)
	      (string<? (symbol->string a) (symbol->string b)))))))

(load "Debugger/debug.sch")
(load "Debugger/trace.sch")

(herald (string-append "\nTwobit heap image dumped on "
		       (unix:current-timestamp)
		       ".\nUsing interpreter for evaluation."))
(dump-interactive-heap "twobit.heap")
(system "larceny -reorganize-and-dump twobit.heap")
(system "mv twobit.heap.split twobit.heap")

; eof
