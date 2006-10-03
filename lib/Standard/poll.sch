; Platform-neutral polling API
; 2004-01-18 / lth
;
; POLL-DESCRIPTORS (input-desc ...) (output-desc ...) block?
;
;   Poll the descriptors for input and/or output, blocking in the
;   polling if block? is true.
;
;   For the purposes of this interface, a descriptor is an object that
;   the I/O system has delivered to the tasking package through the
;   INPUT-NOT-READY-HANDLER and OUTPUT-NOT-READY-HANDLER procedures.
;
; See also
;   tasking-with-io
;   nonblocking-console

(define poll-descriptors)

; The implementation is platform-specific.

(cond-expand
 (unix
  (require 'experimental/poll)))

