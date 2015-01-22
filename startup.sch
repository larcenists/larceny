;;; This file is implicitly loaded by the Larceny REPL
;;; [read-eval-print loop] on startup.

;;; Set up the standard path for the require procedure.
;;;
;;; Affects R7RS, ERR5RS, and R5RS modes.
;;;
;;; WARNING:  This file is ignored by R6RS top-level programs
;;; and Scheme scripts.

(current-require-path
  (list "lib/R7RS"
        "lib/R6RS"
        "lib/SRFI"
	"lib/Ffi"
	"lib/Base"
        "lib/Standard"
        "lib"
	"lib/Debugger"
        "lib/SLIB"))
