; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, big-endian.

(load "Util/sysdep-unix.sch")

(load (make-relative-filename "Util" "nbuild-param-C-be.sch"))
(load (make-relative-filename "Compat" "Larceny" "compat.sch"))
(compat:initialize)
(load (make-relative-filename "Util" "nbuild.sch"))

; eof
