;;; Util/std-heap.sch
;;; Load script for building the full heap image
;;;
;;; BUGS:
;;; - Most compiler names are hidden.  It would be nice to be able to
;;;   expose them in such a way that changes to them would take effect
;;;   (so that this heap could be used for development)
;;; - The FFI internals (loaded from common-heap.sch) are not hidden.

(load "Util/load-env.sch")
(load-environment "Util/modules.list" 'verbose)

(load "Util/common-heap.sch")

(herald (string-append "Heap dumped on "
		       (unix:current-timestamp)
		       ".  Always compiling."))
(dump-interactive-heap "std.heap")
(system "larceny -reorganize-and-dump std.heap")
(system "mv std.heap.split std.heap")

; eof