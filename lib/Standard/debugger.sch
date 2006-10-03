; A stub to load and install the debugger.
; 2004-01-08 / lth
;
; The debugger internals are in the Larceny source tree, so you
; need to install the Larceny sources and you need to configure
; REQUIRE so that it knows where those sources are.

(require 'debugger-internals)
(require 'pretty)

(install-debugger)
