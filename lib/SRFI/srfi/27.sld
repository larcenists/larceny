; SRFI-27: Random bits
; 2004-01-11 / lth
;
; $Id$
;

(define-library (srfi 27 random-bits)

  (export random-integer random-real default-random-source
          make-random-source random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals)

  (import (srfi :27 random-bits)))


(define-library (srfi 27)

  (export random-integer random-real default-random-source
          make-random-source random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals)

  (import (srfi 27 random-bits)))

; eof
