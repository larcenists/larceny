; Ffi/loadffi.sch
; Larceny FFI -- load script
;
; $Id$

(load "Auxlib/values.sch")		; need multiple values

(load "Ffi/memory.sch")                 ; non-conservative collectors
(load "Ffi/tramp.sch")
(load "Ffi/ffi-sparc.sch")
(load "Ffi/ffi-lower.sch")
(load "Ffi/ffi-upper.sch")
(load "Ffi/ffi-sunos4.sch")             ; SunOS 4.x.x

; Default libraries (SunOS 4)

(ffi/libraries '("/usr/lib/libc.so.1.9"))

; eof
