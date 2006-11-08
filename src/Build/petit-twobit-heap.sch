; Load script for building a heap image with all compiler names exposed.
; For Petit Larceny only.  Make sure you start twobit.bin with -stopcopy.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(load "setup.sch")

(setup)

(load-compiler)

(require 'petit-compile-file.sch)

(require 'debug)
(require 'inspect-cont)
(require 'trace)
(install-debugger)

(dump-interactive-heap "twobit.heap")

; eof
