; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, endian-little, 
; MacOS X target OS, Win32 host OS, Petit Larceny host system.

(define *root-directory* "")
(define *sysdep-file* 
  (string-append *root-directory* "Util/sysdep-win32.sch"))

; It should not be necessary to modify any of these.

(load *sysdep-file*)

(load (make-filename *root-directory* "Util" "Configurations" "nbuild-param-C-be-macosx-on-win32.sch"))
(define nbuild-parameter
  (make-nbuild-parameter *root-directory* #t #f #t 
			 "Larceny" "Petit Larceny"))

(load (make-filename *root-directory* "Compat" "Larceny" "compat.sch"))
(compat:initialize)
(load (make-filename *root-directory* "Util" "nbuild.sch"))

; eof
