; Copyright 1998-2004 Lars T Hansen.
;
; $Id$
;
; Petit Larceny:
; Script for building the small heap image "petit-r5rs.heap"
;
; 1  Evaluate (BUILD-R5RS-FILES) in the development environment
; 2  From the command line run
;        petit-r5rs -stopcopy petit.heap
; 3  Load this script.  It will create petit-r5rs.heap.

(load "Auxlib/pp.fasl")
(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

; Dump a unified heap as petit-r5rs.heap

(dump-interactive-heap "petit-r5rs.heap")

; Reorganize and redump the heap and give it the name "petit-r5rs.heap"

(define mv-command)
(define petit-command)

(if (string=? "Win32" (cdr (assq 'os-name (system-features))))
    (begin
      (set! mv-command "rename")
      (set! petit-command ".\\petit-r5rs.bin.exe"))
    (begin
      (set! mv-command "mv")
      (set! petit-command "./petit-r5rs.bin")))

(system (string-append petit-command " -reorganize-and-dump petit-r5rs.heap"))
(system (string-append mv-command " petit-r5rs.heap.split petit-r5rs.heap"))

; eof
