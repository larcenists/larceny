; Util/std-heap.sch
; Load script for building the full heap image
;
; $Id$
;
; BUGS:
; - The FFI internals are not hidden.

(load "Util/load-env.sch")
(load-environment "Util/modules.list" 'verbose)

(if (and (file-exists? "Util/compile-always.fasl")
	 (file-newer? "Util/compile-always.fasl" "Util/compile-always.sch"))
    (load "Util/compile-always.fasl")
    (load "Util/compile-always.sch"))


; Everything will be compiled from now on.

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

(herald (string-append "\nStandard heap image dumped on "
		       (unix:current-timestamp)
		       ".\nUsing compiler for evaluation."))
(dump-interactive-heap "std.heap")
(system "larceny -reorganize-and-dump std.heap")
(system "mv std.heap.split std.heap")

; eof