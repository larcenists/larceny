;;; This file is implicitly loaded by the Larceny REPL
;;; [read-eval-print loop] on startup.

;; Set up the standard path for the require procedure.
(current-require-path
  (list "lib/SRFI"
	"lib/Ffi"
	"lib/Base"
        "lib/Standard" "lib"
	"lib/Debugger"))
