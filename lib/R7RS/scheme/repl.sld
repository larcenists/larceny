;;; FIXME: this isn't right.  It's probably an R5RS-style environment.
;;;
;;; > (interaction-environment)
;;; Error during printing; reverting to the ur-printer.
;;;
;;; > (write (interaction-environment))
;;;
;;; Error: Record access: (environment) is not a record-type-descriptor-type
;;; Entering debugger; type "?" for help.
;;; debug> q

(define-library (scheme repl)

  (export interaction-environment)

  (import (rnrs base)
          (larceny r7rs primitives lowlevel))

  (include "repl.body.scm"))
