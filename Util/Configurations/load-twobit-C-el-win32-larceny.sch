; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, endian-little, 
; Win32 host/target, Petit Larceny host.

(define *root-directory* "")                               ; Win32
(define *sysdep-file* 
  (string-append *root-directory* "Util\\sysdep-win32.sch"))

; It should not be necessary to modify any of these.

(load *sysdep-file*)

(load (make-filename *root-directory* "Util" "Configurations" "nbuild-param-C-el-win32.sch"))
(define nbuild-parameter
  (make-nbuild-parameter *root-directory* #t #f #t 
			 "Larceny" "Petit Larceny"))

(load (make-filename *root-directory* "Compat" "Larceny" "compat.sch"))
(compat:initialize)
(load (make-filename *root-directory* "Util" "nbuild.sch"))

; eof
