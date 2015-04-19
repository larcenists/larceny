;;; SRFI 38: External Representation of Data With Shared Structure
;;;
;;; $Id$
;;;

(define-library (srfi 38 with-shared-structure)

  (export write-with-shared-structure write/ss
          read-with-shared-structure  read/ss)

  (import (srfi :38 with-shared-structure)))


(define-library (srfi 38)

  (export write-with-shared-structure write/ss
          read-with-shared-structure  read/ss)

  (import (srfi 38 with-shared-structure)))

; eof
