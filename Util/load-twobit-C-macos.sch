; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, big-endian, MacOS.

; Only place in the system where the absolute path is needed.

(define *root-directory* "Ertevann:Larceny:Larceny-0.42:src")  ; MACOS
(define *sysdep-file* 
  (string-append *root-directory* ":Util:sysdep-macos.sch"))   ; MACOS

; It should not be necessary to modify any of these.

(load *sysdep-file*)

(load (pathname-append *root-directory* "Util" "nbuild-param-C-macos.sch"))
(define nbuild-parameter
  (make-nbuild-parameter *root-directory* #t #t "Larceny" "Larceny"))

(load (pathname-append *root-directory* "Compat" "Larceny" "compat.sch"))
(compat:initialize)
(load (pathname-append *root-directory* "Util" "nbuild.sch"))

; eof
