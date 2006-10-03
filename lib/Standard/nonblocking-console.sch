; Nonblocking console ports
; 2004-01-18 / lth
;
; Defines two global thunks:
;
; NONBLOCKING-CONSOLE-INPUT-PORT  =>  input-port
;
;   Produces a fresh console input port when called; this port is
;   nonblocking and when an attempt is made to read from it in such a
;   way that the reading would block, the port handler will call the
;   procedure INPUT-NOT-READY-HANDLER with an object that allows the
;   polling code to determine when the port is again ready for
;   reading.
;
; NONBLOCKING-CONSOLE-OUTPUT-PORT  =>  output-port
;
;   Ditto for the console output port; if writing to the port would 
;   block, the procedure OUTPUT-NOT-READY-HANDLER is called with an
;   object that can be used by the polling code.
;
; See also:
;   poll
;   tasking-with-io

(define nonblocking-console-input-port)
(define nonblocking-console-output-port)

; The implementation is platform-specific.

(cond-expand
 (unix
  (require 'experimental/nonblocking-console)))


