; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Syscall ID definitions.
;
; FIXME: These values (may) belong in a configuration file.
; NOTE:  Values correspond to the table ordering in Rts/Sys/cglue.c.

(define syscall:open 0)
(define syscall:unlink 1)
(define syscall:close 2)
(define syscall:read 3)
(define syscall:write 4)
(define syscall:get-resource-usage 5)
(define syscall:dump-heap 6)
(define syscall:exit 7)
(define syscall:mtime 8)
(define syscall:access 9)
(define syscall:rename 10)
(define syscall:pollinput 11)
(define syscall:getenv 12)
(define syscall:gc 13)
(define syscall:flonum-log 14)
(define syscall:flonum-exp 15)
(define syscall:flonum-sin 16)
(define syscall:flonum-cos 17)
(define syscall:flonum-tan 18)
(define syscall:flonum-asin 19)
(define syscall:flonum-acos 20)
(define syscall:flonum-atan 21)
(define syscall:flonum-atan2 22)
(define syscall:flonum-sqrt 23)
(define syscall:stats-dump-on 24)
(define syscall:stats-dump-off 25)
(define syscall:iflush 26)
(define syscall:gcctl 27)
(define syscall:block-signals 28)
(define syscall:flonum-sinh 29)
(define syscall:flonum-cosh 30)
(define syscall:system 31)
(define syscall:C-ffi-apply 32)
(define syscall:C-ffi-dlopen 33)
(define syscall:C-ffi-dlsym 34)
(define syscall:make-nonrelocatable 35)
(define syscall:object->address 36)
(define syscall:ffi-getaddr 37)
(define syscall:sro 38)
(define syscall:sys-feature 39)
(define syscall:peek-bytes 40)
(define syscall:poke-bytes 41)
(define syscall:segment-code-address 42) ; Petit Larceny only

; eof
