; SRFI 59: Vicinities
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-59/srfi-59.html> for the full document.

(define-library (srfi 59 vicinities)

  (export program-vicinity library-vicinity implementation-vicinity
          user-vicinity home-vicinity in-vicinity sub-vicinity
          make-vicinity pathname->vicinity vicinity:suffix?)

  (import (srfi :59 vicinities)))

(define-library (srfi 59)

  (export program-vicinity library-vicinity implementation-vicinity
          user-vicinity home-vicinity in-vicinity sub-vicinity
          make-vicinity pathname->vicinity vicinity:suffix?)

  (import (srfi 59 vicinities)))

; eof
