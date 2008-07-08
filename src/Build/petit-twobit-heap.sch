; Load script for building a heap image with all compiler names exposed.
; For Petit Larceny only.  Make sure you start twobit.bin with -stopcopy.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(load "setup.sch")

(setup)

(load-compiler)

(require 'petit-compile-file)

(require 'debug)
(require 'inspect-cont)
(require 'trace)
(install-debugger)

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

(dump-interactive-heap "twobit.heap")

; eof
