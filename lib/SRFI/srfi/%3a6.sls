; SRFI 6: Basic string ports.
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-6/srfi-6.html> for the full document.

(library (srfi :6 basic-string-ports)

  (export open-input-string open-output-string get-output-string)

  (import (primitives
           open-input-string open-output-string get-output-string)))

(library (srfi :6)

  (export open-input-string open-output-string get-output-string)

  (import (srfi :6 basic-string-ports)))

; eof
