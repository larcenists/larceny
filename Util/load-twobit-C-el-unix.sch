; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, endian-little, Unix.

(define *root-directory* "")                               ; Unix
(define *sysdep-file* 
  (string-append *root-directory* "Util/sysdep-unix.sch")) ; Unix-like

; It should not be necessary to modify any of these.

(load *sysdep-file*)

(load (pathname-append *root-directory* "Util" "nbuild-param-C-el-unix.sch"))
(define nbuild-parameter
  (make-nbuild-parameter *root-directory* #f #f "Larceny" "Larceny"))

(load (pathname-append *root-directory* "Compat" "Larceny" "compat.sch"))
(compat:initialize)
(load (pathname-append *root-directory* "Util" "nbuild.sch"))

; eof
