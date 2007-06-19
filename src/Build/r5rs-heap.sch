; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC Larceny
; Script for building the small heap image "r5rs.heap"
;
; 1  Evaluate (BUILD-R5RS-FILES) in the development environment
; 2  From the command line run
;        larceny.bin -stopcopy petit.heap
; 3  Load this script.  It will create r5rs.heap.

(load "Auxlib/pp.fasl")
(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

(dump-interactive-heap "r5rs.heap")
(cond
 ((equal? (cdr (assq 'os-name (system-features)))
	  "Win32")
  (system "larceny.bin -reorganize-and-dump -heap r5rs.heap")
  (system "move r5rs.heap.split r5rs.heap"))
 (else
  (system "./larceny.bin -reorganize-and-dump -heap r5rs.heap")
  (system "/bin/mv r5rs.heap.split r5rs.heap")))

; eof
