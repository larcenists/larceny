; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Load script for twobit with Standard-C assembler, endian-little, 
; Win32 host/target, Petite Chez Scheme host.

(define *root-directory* "")                               ; Win32
(define *sysdep-file* 
  (string-append *root-directory* "Util\\sysdep-win32.sch"))

; It should not be necessary to modify any of these.

(load *sysdep-file*)

(load (make-filename *root-directory* "Util" "nbuild-param-C-el-win32.sch"))
(define nbuild-parameter
  (make-nbuild-parameter *root-directory* #f #t #t 
			 "Petite" "Petite Chez Scheme"))

(load (make-filename *root-directory* "Compat" "Petite" "compat.sch"))
(compat:initialize)
(load (make-filename *root-directory* "Util" "nbuild.sch"))

; eof
