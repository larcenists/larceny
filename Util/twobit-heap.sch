; Util/twobit-heap.sch
; Load script for building a heap image with all compiler names exposed.
;
; $Id: twobit-heap.sch,v 1.1.1.1 1998/11/19 21:51:56 lth Exp $

(load "Util/load-twobit.sch")

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
