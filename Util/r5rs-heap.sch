; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Script to dump r5rs.heap -- the small heap image.

(load "Auxlib/pp.fasl")
(repl-printer
 (lambda (x)
   (if (not (eq? x (unspecified)))
       (pretty-print x))))

(dump-interactive-heap "r5rs.heap")
(system "./larceny.bin -reorganize-and-dump r5rs.heap")
(system "/bin/mv r5rs.heap.split r5rs.heap")

; eof
