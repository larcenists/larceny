; Copyright 1998-2004 Lars T Hansen.
;
; $Id$
;
; Script to dump petit-r5rs.heap -- the small heap image.
; Petit Larceny only

(load "Auxlib/pp.fasl")
(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

; Dump a unified heap as petit-r5rs.heap

(dump-interactive-heap "petit-r5rs.heap")

; Reorganize and redump the heap and give it the name "petit-r5rs.heap"

(define mv-command)
(define petit-command)

(if (string=? "Win32" (cdr (assq 'os-name (system-features))))
    (begin
      (set! mv-command "rename")
      (set! petit-command ".\\petit-r5rs.exe"))
    (begin
      (set! mv-command "mv")
      (set! petit-command "./petit-r5rs")))

(system (string-append petit-command " -reorganize-and-dump petit-r5rs.heap"))
(system (string-append mv-command " petit-r5rs.heap.split petit-r5rs.heap"))

; eof
